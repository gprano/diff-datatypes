(** An implementation of tree-shaped data-structure *)

exception Empty

type error = [ `Todo | `Read_none ]

exception Error of error

module type S = sig

  include Irmin.Contents.S
  
  type elt
  
  val create : unit -> t Lwt.t
  
  val add : t -> elt -> t list -> t Lwt.t

  val read_exn : t -> (elt option * t list) Lwt.t

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

