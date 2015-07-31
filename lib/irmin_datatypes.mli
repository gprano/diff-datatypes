
module type MSTACK = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop : t -> (elt option * t) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
end

module type MTREE = sig
  include Irmin.Contents.S
  type elt
  val build : elt option -> t list -> t Lwt.t
  val destr : t -> (elt option * t list) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
end

module MSTACK_Make (S: Irmin_heap.S) : MSTACK
  with type t = S.t
   and type elt = S.elt
   and module Path = S.Path

module MQUEUE_Make (S: Irmin_heap.S) : MSTACK
  with type t = S.t
   and type elt = S.elt
   and module Path = S.Path

module MTREE_Make (S: Irmin_heap.S) : MTREE
  with type t = S.t
   and type elt = S.elt
   and module Path = S.Path
