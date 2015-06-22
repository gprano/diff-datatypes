open Lwt
open Irmin.Merge.OP

exception Empty

type error = [ `Todo ]
             
exception Error of error

module type S = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val add : t -> elt -> t list -> t Lwt.t
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

  module Path = P

  module C = struct (* what will be inside the AO store *)

    type node = {
      elt : K.t option;
      children : K.t list;
    }

    module KO = Tc.Option (K)
    module KL = Tc.List (K)
    module Node = Tc.Biject
        (Tc.Pair(KO)(KL))
        (struct
          type t = node
          let to_t (elt,children) =
            {elt; children}
          let of_t {elt; children} =
            (elt, children)
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

  type index = K.t (* must always point to a C.Node and not C.Elt ? *)
  include K
  
  type elt = V.t

  let empty = {
    C.elt = None;
    C.children = [];
  }

  let create () =
    Store.create () >>= fun store ->
    Store.add (store "create") (C.Node empty) >>= fun root ->
    return root

  let add s elt children =
    Store.create () >>= fun store ->
    Store.add (store "add elt") (C.Elt elt) >>= fun key_elt ->
    let node = {
      C.elt = Some key_elt;
      C.children = children;
    } in
    Store.add (store "add node") (C.Node node) >>= fun key_node ->
    return key_node

  type edit =
    | Ins of index
    | Cpy of index
    | Del of index

  type edit_script = edit list

  module ObjSet = Set.Make (K)

  type dfs_ctxt =
    {visited : ObjSet.t; todo : t list}

  let rec dfs_1step store {visited; todo} =
    match todo with
    | [] -> return {visited; todo}
    | obj::xs ->
      if ObjSet.mem obj visited then dfs_1step store {visited; todo = xs}
      else
        begin
          Store.read (store "read dfs_1step") obj >>= function
          | None -> failwith "K.t pointer to nothing"
          | Some (C.Node node) ->
            return {visited = ObjSet.add obj visited; todo = node.C.children@xs}
          | Some (C.Elt _) -> return {visited; todo}
        end

  let diff (s1 : t) (s2 : t) : edit_script Lwt.t =
    Store.create() >>= fun store ->
    
    let rec diff_ctxt c1 c2 =
      match (c1.todo, c2.todo) with
      | [], [] -> return []
      | [], y::_ ->
        dfs_1step store c2 >>= fun c2 ->
        diff_ctxt c1 c2 >>= fun es ->
        return (Ins y :: es)
      | x::_, [] ->
        dfs_1step store c1 >>= fun c1 ->
        diff_ctxt c1 c2 >>= fun es ->
        return (Ins x :: es)
      | x::_, y::_ ->
        dfs_1step store c1 >>= fun c1_ ->
        dfs_1step store c2 >>= fun c2_ ->
        let best2 () =
          diff_ctxt c1_ c2 >>= fun es1 ->
          diff_ctxt c1 c2_ >>= fun es2 ->
          let l1 = Del x :: es1 in
          let l2 = Ins y :: es2 in
          if List.length l1 < List.length l2 then return l1 else return l2 in
        let best3 () =
          diff_ctxt c1_ c2_ >>= fun es ->
          let l1 = Cpy x :: es in
          best2 () >>= fun l2 ->
          if List.length l1 < List.length l2 then return l1 else return l2 in
        if equal x y then best3 () else best2 () in

    let c1 = {visited = ObjSet.empty; todo = [s1]} in
    let c2 = {visited = ObjSet.empty; todo = [s2]} in
    diff_ctxt c1 c2

  let rec patch_core c stk = function
    | [] -> return ()
    | _ -> failwith "todo"
  (* There is a problem when patching trees rather than lists; 
     if we have subtrees insertions on both sides it's not clear to merge in a valid way *)
  
  let patch : t -> edit_script -> t = raise (Error `Todo)

  let merge_script : edit_script -> edit_script -> edit_script =
    raise (Error `Todo)

  let merge : Path.t -> t option Irmin.Merge.t =
    
    let merge ~old s1 s2 =
      old () >>= function
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) ->
        diff old s1 >>= fun e1 ->
        diff old s2 >>= fun e2 ->
        let e = merge_script e1 e2 in
        ok (patch old e) in

    fun _path -> Irmin.Merge.option (module K) merge
  
end
