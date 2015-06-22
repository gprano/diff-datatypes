(** An implementation of tree-shaped data-structure *)

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
    (C: Config)
  : S with type elt = V.t
       and module Path = P
