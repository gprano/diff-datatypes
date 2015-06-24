


module type MSTACK = sig
  type t
  type elt
  val empty : t Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop : t -> (elt option * t) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t

end

module MSTACK_Make (S: Irmin_heap.S) : MSTACK
