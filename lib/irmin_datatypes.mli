module type MSTACK = sig
  type t
  type elt
  val empty : t Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop : t -> (elt option * t) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
  module Path : Irmin.Path.S
  val merge : Path.t -> t option Irmin.Merge.t
end

module MSTACK_Make (S: Irmin_heap.S) : MSTACK
  with type t = S.t
   and type elt = S.elt
   and module Path = S.Path
