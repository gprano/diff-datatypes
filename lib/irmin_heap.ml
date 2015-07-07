open Lwt
open Irmin.Merge.OP

exception Empty

type error = [ `Todo | `Read_none ]

exception Error of error

module type S = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val build : elt option -> t list -> t Lwt.t
  val read_exn : t -> (elt option * t list) Lwt.t
  val to_list : t -> elt list Lwt.t
  val of_list: elt list -> t Lwt.t
  val show: (elt -> string) -> t -> string Lwt.t
  val merge3: old:(unit -> [< `Conflict of 'a | `Ok of t option ] Lwt.t) -> t -> t -> t Irmin.Merge.result Lwt.t
end

module type Config = sig
  val conf: Irmin.config
  val task: string -> Irmin.task
end

module Make
    (AO: Irmin.AO_MAKER)
    (K: Irmin.Hash.S)
    (V: Tc.S0)
    (P: Irmin.Path.S)
    (Config: Config)
= struct

  type elt = V.t
  module K = K
  include K

  type node = { value : t option; children : t list}

  module Path = P

  module C = struct

    module KO = Tc.Option (K)
    module KL = Tc.List (K)
    module Node = Tc.Biject
        (Tc.Pair(KO)(KL))
        (struct
          type t = node
          let to_t (value,children) =
            {value; children}
          let of_t {value; children} =
            (value, children)
        end)

    type t =
      | Node of Node.t
      | Elt of V.t
    with compare

    let equal_node n1 n2 =
      Node.compare n1 n2 = 0

    let to_json = function
      | Node n -> `O [ "node" , Node.to_json n ]
      | Elt e  -> `O [ "elt"  , V.to_json e ]

    let of_json = function
      | `O [ "node" , j ] -> Node (Node.of_json j)
      | `O [ "elt"  , j ] -> Elt (V.of_json j)
      | j -> Ezjsonm.parse_error j "C.of_json"

    let equal x y = match x, y with
      | Node x, Node y -> Node.equal x y
      | Elt x, Elt y -> V.equal x y
      | _ -> false

    let hash = Hashtbl.hash

    let to_string t = Ezjsonm.to_string (to_json t) 
    let of_string s = of_json (Ezjsonm.from_string s)
    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len
    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string
    let size_of t =
      let str = to_string t in
      String.length str

  end

  module Store = struct

    module S = AO(K)(C)

    include S

    let create () =
      create Config.conf Config.task

  end

  let empty = {
    value = None;
    children = [];
  }

  let create () =
    Store.create () >>= fun store ->
    Store.add (store "create") (C.Node empty) >>= fun root ->
    return root

  let build elt children =
    Store.create () >>= fun store ->
    ( match elt with
      | None -> return None
      | Some elt -> Store.add (store "add elt") (C.Elt elt) >>= fun k ->
        return (Some k) ) >>= fun value ->
    let node = {value;children;} in
    Store.add (store "add node") (C.Node node) >>= fun key_node ->
    return key_node

  let read_exn : t -> (elt option * t list) Lwt.t = fun s ->
    Store.create () >>= fun store ->
    Store.read (store "read") s >>= function
    | None -> raise (Error `Read_none)
    | Some (C.Elt _) -> failwith "try to read Elt from API"
    | Some (C.Node x) -> ( match x.value with
        | None -> return (None, x.children)
        | Some k ->
          ( Store.read (store "read elt") k >>= function
              | None -> raise (Error `Read_none)
              | Some (C.Node _) -> failwith "read elt key and get a node"
              | Some (C.Elt elt) -> return (Some elt, x.children) ))

  let shorthash k = String.sub (to_hum k) 0 5

  let rec show to_string s =
    read_exn s >>= fun (elt, l) ->
    let str_elt = match elt with
      | None -> "None"
      | Some e -> to_string e in
    let rec f = function
      | [] -> return ""
      | x::xs -> show to_string x >>= fun str ->
        f xs >>= fun str2 -> return ("("^str^")"^str2) in
    f l >>= fun str ->
    return (str_elt^"<"^(shorthash s)^">"^":"^str)

  let share_equal x y =
    read_exn x >>= fun (ex,lx) ->
    read_exn y >>= fun (ey,ly) ->
    return (ex = ey && List.length lx = List.length ly)

  type edit =
    | Ins of t
    | Cpy of t
    | Del of t

  type edit_script = edit list

  let string_of_edit = function
    |Ins k -> "Ins "^(shorthash k)
    |Cpy k -> "Cpy "^(shorthash k)
    |Del k -> "Del "^(shorthash k)

  let string_of_es es = String.concat "," (List.map string_of_edit es)
  let print_es es = Printf.printf "%d: %s\n" (List.length es) (string_of_es es)

  type dfs_ctxt = t list

  let rec dfs_1step store todo =
    match todo with
    | [] -> return todo
    | obj::xs ->
      begin
        Store.read (store "read dfs_1step") obj >>= function
        | None -> failwith "K.t pointer to nothing"
        | Some (C.Node node) ->
          return (node.children@xs)
        | Some (C.Elt _) -> return todo
      end

  let diff (s1 : t) (s2 : t) : edit_script Lwt.t =
    let h = Hashtbl.create 10 in
    Store.create() >>= fun store ->

    let rec diff_ctxt c1 c2 i1 i2 =
      if Hashtbl.mem h (c1,c2) then
        return (Hashtbl.find h (c1,c2))
      else
        begin
          match (c1, c2) with
          | [], [] -> return []
          | [], y::_ ->
            dfs_1step store c2 >>= fun c2 ->
            diff_ctxt c1 c2 i1 (i2+1) >>= fun es ->
            return (Ins y :: es)
          | x::_, [] ->
            dfs_1step store c1 >>= fun c1 ->
            diff_ctxt c1 c2 (i1+1) i2 >>= fun es ->
            return (Del x :: es)
          | x::_, y::_ ->
            dfs_1step store c1 >>= fun c1_ ->
            dfs_1step store c2 >>= fun c2_ ->
            let best2 () =
              diff_ctxt c1_ c2 (i1+1) i2 >>= fun es1 ->
              diff_ctxt c1 c2_ i1 (i2+1) >>= fun es2 ->
              let l1 = Del x :: es1 in
              let l2 = Ins y :: es2 in
              if List.length l1 < List.length l2 then return l1 else return l2 in
            let best3 () =
              diff_ctxt c1_ c2_ (i1+1) (i2+1) >>= fun es ->
              let l1 = Cpy x :: es in
              best2 () >>= fun l2 ->
              if List.length l1 < List.length l2 then return l1 else return l2 in
            share_equal x y >>= fun b -> 
            if b then best3 () else best2 ()
        end >>= fun res ->
        Hashtbl.add h (c1,c2) res;
        return res in
    let c1 = [s1] in
    let c2 = [s2] in
    diff_ctxt c1 c2 0 0
  
let rec patch_core store (c : dfs_ctxt) =
  let add_node_of_key x es c =
    Store.read (store "patch_core read") x >>= function
    | None -> failwith "read None"
    | Some (C.Elt _ ) -> failwith "Ins C.Elt"
    | Some (C.Node node) ->
      begin
        let rec f c e acc = function
          | [] -> return (c,e,acc)
          | x :: xs ->
            patch_core store c e >>= fun (c,e,k) ->
            f c e (acc@[k]) xs in
        f c es [] node.children >>= fun (c,e,acc) ->
        let newnode = { value = node.value ;
                        children = acc; } in
        Store.add (store "patch_core add") (C.Node newnode) >>= fun k ->
        return (c,e,k)
      end in
  function
  | [] -> failwith "call to patch_core with an empty edit_script"
  | x::y ->
    begin
      (* Printf.printf "e %s c %s\n" (string_of_edit x)
         (match c with [] -> "[]" | e::x -> shorthash e); *)
      match x::y with
      | Ins x::es -> add_node_of_key x es c
      | Del _::es -> dfs_1step store c >>= fun c -> patch_core store c es
      | Cpy _::es -> ( match c with
          | [] -> failwith "patch_core : edit script not compatible"
          | x::_ -> dfs_1step store c >>= fun c -> add_node_of_key x es c )
      | _-> failwith "no"
    end
      
let patch : t -> edit_script -> t Lwt.t = fun k es ->
  Store.create () >>= fun store ->
  patch_core store [k] es >>= fun (c,e,k) ->
  if not (List.for_all (function Del _ -> true | _ -> false) e)
  then ( print_es e; Printf.printf "%d\n" (List.length c));
  return k

(* TODO fix equality used for sharing*)
let rec merge_script : edit_script -> edit_script -> edit_script Lwt.t =
  fun a b -> match a,b with
    | [],[] -> return []
    | Ins ex :: xs, Ins ey :: ys ->
      share_equal ex ey >>= fun eq ->
      if eq then
        merge_script xs ys >>= fun es ->
        Ins ex :: es |> return
      else
        merge_script xs b >>= fun es ->
        Ins ex :: es |> return
    | Ins e :: xs, ys
    | xs, Ins e :: ys ->
      merge_script xs ys >>= fun es -> Ins e :: es |> return
    | Del ex :: xs, Del ey :: ys -> assert (ex=ey);
      merge_script xs ys >>= fun es -> 
      Del ex :: es |> return
    | Del e :: xs, Cpy e_ :: ys
    | Cpy e_ :: xs, Del e :: ys -> assert(e = e_);
      merge_script xs ys >>= fun es ->
      Del e :: es |> return
    | Cpy ex :: xs, Cpy ey :: ys -> assert (ex=ey);
      merge_script xs ys >>= fun es ->
      Cpy ex :: es |> return
    | ex::xs,[] | [],ex::xs ->
      failwith "Del x,[] or Cpy x,[]: this should not happen"

let merge3 ~old s1 s2 =
    old () >>= function
    | `Conflict _ | `Ok None -> conflict "merge"
    | `Ok (Some old) ->
      diff old s1 >>= fun e1 ->
      diff old s2 >>= fun e2 ->
      merge_script e1 e2 >>= fun e ->
      patch old e >>= fun s_merge ->

      (* let f = fun x -> string_of_int (Obj.magic x : int) in *)
      (* let g = fun x -> return (print_endline x) in *)
      (* List.map (fun x -> show f x >>= g) [old;s1;s2;s_merge] *)
      (* |> Lwt.join >>= fun () -> *)
      (* List.iter print_es [e1;e2;e]; *)
      (* List.map (fun x -> patch old x >>= fun x -> show f x >>= g) [e1;e2] *)
      (* |> Lwt.join >>= fun () -> *)
      ok s_merge 

let merge : Path.t -> t option Irmin.Merge.t =
  fun _path -> Irmin.Merge.option (module K) merge3

let rec to_list s =
  read_exn s >>= function
  | Some e, [x] -> to_list x >>= fun x -> return (e::x)
  | None, [] -> return []
  | _ -> failwith "incorrect list shape"

let rec of_list = function
  | [] -> build None []
  | e::x -> of_list x >>= fun s -> build (Some e) [s]

end
