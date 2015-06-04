(** Datatypes for testing diff-based merging *)

module type IQUEUE =
  sig
    type 'a t
    val empty : 'a t
    val push : 'a t -> 'a -> 'a t
    val pop : 'a t -> 'a option * 'a t
  end

module IStack : IQUEUE

module IQueue : IQUEUE

module Tree :
  sig
    type 'a t = Leaf | Node of 'a t * 'a * 'a t
    val show : ('a -> string) -> 'a t -> string
  end
