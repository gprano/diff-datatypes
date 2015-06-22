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


  let (merge : Path.t -> t option Irmin.Merge.t) = raise (Error `Todo)

  
end
