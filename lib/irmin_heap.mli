(** An implementation of tree-shaped data-structure *)

exception Empty

type error = [ `Todo ]

exception Error of error

module type S = sig

  type elt
  type key
  
  type node = { value: key option; children : key list }

  include Irmin.Contents.S
  
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
       and type key = K.t
       and module Path = P

