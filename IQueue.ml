open Printf

module type HEAP = sig

  type edit_script

  val string_of_edit_script : edit_script -> string

  val diff : 'a -> 'a -> edit_script

  val patch : 'a -> edit_script -> 'a

  type 'b action =
    {on_entry : 'b -> Obj.t -> 'b;
     on_exit  : 'b -> Obj.t -> 'b;
     on_cross : 'b -> Obj.t -> 'b}

  val dfs : Obj.t -> 'b action -> 'b -> 'b

  val show_obj : 'a -> unit
end

module Heap : HEAP = struct

  type edit_info = int * (int * int) list

  let string_of_edit_info (arity, vinfo) =
    String.concat "" @@
    sprintf "%d," arity ::
    List.map (fun (p,v) -> sprintf "{%x,%x} " p v) vinfo

  type edit =
  | Ins of edit_info
  | Cpy of edit_info
  | Del of edit_info

  let string_of_edit = function
    | Ins ei -> String.concat "" ["Ins "; string_of_edit_info ei]
    | Cpy ei -> String.concat "" ["Cpy "; string_of_edit_info ei]
    | Del ei -> String.concat "" ["Del "; string_of_edit_info ei]

  type edit_script = edit list

  let string_of_edit_script es =
    String.concat "\n" @@ List.map (fun e -> string_of_edit e) es

  type 'a action =
    {on_entry : 'a -> Obj.t -> 'a;
     on_exit  : 'a -> Obj.t -> 'a;
     on_cross : 'a -> Obj.t -> 'a}

   module OrdObj = struct
     include Obj
     let compare v1 v2 =
       compare (Obj.magic v1 : int) (Obj.magic v2 : int)
   end

   module ObjSet = Set.Make (OrdObj)

   let dfs obj action init =
     let visited = ref ObjSet.empty in
     let rec dfs_main obj acc =
       if Obj.is_int obj then acc
       else if ObjSet.mem obj !visited then
         action.on_cross acc obj
       else
         let () = visited := ObjSet.add obj !visited in
         let acc = action.on_entry acc obj in
         let acc =
           (* Regular objects *)
           if Obj.tag obj == 0 then
             let rec loop idx acc =
               if idx == Obj.size obj then acc
               else loop (idx + 1) (dfs_main (Obj.field obj idx) acc) in
             loop 0 acc
           (* Object types not known to contain pointers *)
           else if Obj.tag obj == Obj.string_tag ||
                   Obj.tag obj == Obj.double_tag ||
                   Obj.tag obj == Obj.double_array_tag ||
                   Obj.tag obj == Obj.no_scan_tag then acc
           (* object type not recognized *)
           else failwith "Heap.dfs : Unhandled object type" in
         action.on_exit acc obj
     in dfs_main obj init

  type dfs_ctxt =
    {visited : ObjSet.t; todo : Obj.t list}

  let rec dfs_1step {visited; todo} =
    match todo with
    | [] -> {visited; todo}
    | obj::xs ->
        if Obj.is_int obj || Obj.tag obj <> 0 then
          failwith "Heap.dfs_1step : input must be object typed"
        else if ObjSet.mem obj visited then dfs_1step {visited; todo = xs}
        else
          let todo =
            let rec loop idx todo =
              if idx < 0 then todo
              else
                let next = Obj.field obj idx in
                (* ignore non-objects *)
                if Obj.is_int next || Obj.tag next <> 0
                then loop (idx - 1) todo
                else loop (idx - 1) (next::todo)
            in loop (Obj.size obj - 1) xs
          in {visited = ObjSet.add obj visited; todo = todo}

  let diff o1 o2 =
    let proc_obj obj =
      let rec loop idx (arity, vlist) =
        if idx < 0 then (arity, vlist)
        else
          let f = Obj.field obj idx in
          if Obj.is_int f then
            loop (idx - 1) (arity, (idx, (Obj.magic f : int))::vlist)
          else if Obj.tag f <> 0 then
            failwith "Heap.diff : cannot handle non-standard objects"
          else loop (idx - 1) (arity+1, vlist) in
      let (arity, vlist) = loop (Obj.size obj - 1) (0, []) in
      (arity, vlist)
    in

    let rec diff_ctxt c1 c2 =
      match (c1.todo, c2.todo) with
      | ([], []) -> []
      | ([], y::_) ->
          Ins (proc_obj y) :: (diff_ctxt c1 (dfs_1step c2))
      | (x::_,[]) ->
          Del (proc_obj x) :: (diff_ctxt (dfs_1step c1) c2)
      | (x::_,y::_) ->
          let best2 () =
            let l1 = Del (proc_obj x) :: (diff_ctxt (dfs_1step c1) c2) in
            let l2 = Ins (proc_obj y) :: (diff_ctxt c1 (dfs_1step c2)) in
            if List.length l1 < List.length l2 then l1 else l2 in
          let best3 () =
            let l1 = Cpy (proc_obj x) :: (diff_ctxt (dfs_1step c1) (dfs_1step c2)) in
            let l2 = best2 () in
            if List.length l1 < List.length l2 then l1 else l2 in
          let px = proc_obj x in
          let py = proc_obj y in
          if px = py then best3 () else best2 ()
    in
    let c1 = {visited = ObjSet.empty; todo = [Obj.repr (ref o1)]} in
    let c2 = {visited = ObjSet.empty; todo = [Obj.repr (ref o2)]} in
    diff_ctxt c1 c2


  let rec patch_core c stk = function
  | [] -> ()
  | Ins (a,vl)::es ->
      let size = a + List.length vl in
      let b = Obj.new_block 0 size in
      let () = List.iter (fun (p,v) -> Obj.set_field b p (Obj.repr v)) vl in
      let setf = Stack.pop stk in
      let () = setf b in
      let rec loop n = function
      | [] ->
          if n < 0 then ()
          else
            (Stack.push (Obj.set_field b n) stk;
             loop (n - 1) [])
      | (p,v)::xs ->
          if p <> n then
            (Stack.push (Obj.set_field b n) stk;
             loop (n - 1) ((p,v)::xs))
          else loop (n - 1) xs
      in
       loop (size - 1) (List.rev vl);
       patch_core c stk es
  | Del _::es -> patch_core (dfs_1step c) stk es
  | Cpy _::es ->
      match c.todo with
      | [] -> failwith "patch : edit script not compatible"
      | orig::_ ->
          let obj = Obj.dup orig in
          let setf = Stack.pop stk in
          let () = setf obj in
          let rec loop idx =
            if idx < 0 then ()
            else
              let f = Obj.field obj idx in
              if Obj.is_int f then loop (idx-1)
              else
                (Stack.push (Obj.set_field obj idx) stk;
                 loop (idx-1))
          in
            loop (Obj.size obj - 1);
            patch_core (dfs_1step c) stk es

  let patch v es =
    let rv = ref v in
    let res = ref rv in
    let p = Obj.set_field (Obj.repr res) 0 in
    let stk = Stack.create () in
    let () = Stack.push p stk in
    patch_core {visited = ObjSet.empty; todo = [Obj.repr rv]} stk es;
    !(!res)

  let show_obj q =
    let on_entry () o =
      printf "%x[" (Obj.magic o : int);
      for i = 0 to Obj.size o - 1 do
        printf "%x " (Obj.magic (Obj.field o i) : int);
      done;
      printf "]\n" in
    let noop () _ = () in
    let acts = {on_entry; on_exit = noop; on_cross = noop} in
    dfs (Obj.repr q) acts ();
    printf "\n"
end

module type IQUEUE = sig
  type 'a t
  val empty : 'a t
  val push  : 'a t -> 'a -> 'a t
  val pop   : 'a t -> 'a option * 'a t
end

module IQueue : IQUEUE = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let push (front, back) v = (front, v::back)

  let pop (front, back) =
    match front with
    | [] -> (match List.rev back with
             | [] -> (None, ([],[]))
             | x::xs -> (Some x, (xs, [])))
    | x::xs -> (Some x, (xs, back))
end

module Tree = struct
  type 'a t = Leaf | Node of 'a t * 'a * 'a t

  let rec show to_str = function
  | Leaf -> "Leaf"
  | Node (l,v,r) ->
      String.concat "" ["("; show to_str l; ",";
                        to_str v;
                        ","; show to_str r; ")"]

end

let q1 = IQueue.empty
let () = Heap.show_obj q1
let q2 = IQueue.push q1 0
let () = Heap.show_obj q2
let es = Heap.diff q1 q2
let () = print_string @@ Heap.string_of_edit_script es
let () = print_string "\n\n"

let () = Heap.show_obj (ref [])
let () = Heap.show_obj (ref [1])
let () = Heap.show_obj (ref [2])
let () = print_string "This -->"
let () = print_string @@
         Heap.string_of_edit_script @@
         Heap.diff [] [1]
let () = print_string "\n\n"

open Tree

let () = printf "----------\n"

let t1 = Node (Node (Leaf, 1, Leaf) , 0, Node (Leaf, 2, Leaf))
let t2 = Node (Node (Leaf, 1, Leaf) , 0, Node (Node (Leaf, 3, Leaf), 2, Node (Leaf, 4, Leaf)))
let es = Heap.diff t1 t2
let () = print_string @@ Heap.string_of_edit_script es
let () = print_string "\n\n"
let t3 = Heap.patch t1 es
let () = print_string @@ show string_of_int t3
let () = print_string "\n"

let l1 = []
let l2 = [1]
let es = Heap.diff l1 l2
let () = print_string @@ Heap.string_of_edit_script es
let () = print_string "\n"
let l3 = Heap.patch l1 es
let () = List.iter (fun i -> printf "%d," i) l3
let () = print_string "\n"
