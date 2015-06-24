open Lwt


module type MSTACK = sig
  type t
  type elt
  val empty : t Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop : t -> (elt option * t) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
end

module MSTACK_Make (S: Irmin_heap.S)
= struct
  
  type t = S.t
  type elt = S.elt

  let empty = S.create ()

  let push s elt = S.add s elt [s]

  let pop s =
    S.read_exn s >>= function
    | Some e, [x] -> return (Some e, x)
    | None, [] -> return (None,s) 
    | _ -> failwith "incorrect stack shape"

  let rec show to_string s =
    S.read_exn s >>= function
    | Some e, [x] ->
      ( show to_string x >>= fun str ->
        return ((to_string e)^"::"^str) )
    | None, [] -> return ""
    | _ -> failwith "incorrect stack shape"

end
