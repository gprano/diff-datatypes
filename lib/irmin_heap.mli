(** An implementation of tree-shaped data-structure *)

exception Empty

type error = [ `Todo | `Read_none ]

exception Error of error

module type S = sig

  (** A tree structure, with content stored in nodes and arbitrary arity *)

  include Irmin.Contents.S

  (** The type of content stored in the nodes *)
  type elt
  
  val create : unit -> t Lwt.t

  (** Creates a node of the tree, with an optional content and a list of children *)
  val build : elt option -> t list -> t Lwt.t

  (** Reads a node of the tree *)
  val read_exn : t -> (elt option * t list) Lwt.t

  (** Fails if the tree is not a linked list *)
  val to_list : t -> elt list Lwt.t

  val of_list: elt list -> t Lwt.t

  (** Prints the structure to a string *)
  val show: (elt -> string) -> t -> string Lwt.t

  (** only used if merge has to be redefined, for instance to normalize the structure before *)
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
    (C: Config)
  : S with type elt = V.t
       and type t = K.t
       and module Path = P

