open Lwt

module type MSTACK = sig
  type t
  type elt
  val create : unit -> t Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop : t -> (elt option * t) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
  module Path : Irmin.Path.S
  val merge : Path.t -> t option Irmin.Merge.t
end

module MSTACK_Make (S: Irmin_heap.S)
= struct
  
  type t =  S.t
  type elt = S.elt
  module Path = S.Path
  let create = S.create
  let merge = S.merge

  let push s elt = S.build (Some elt) [s]

  let pop s =
    S.read_exn s >>= function
    | Some e, [x] -> return (Some e, x)
    | None, [] -> return (None,s) 
    | _ -> failwith "incorrect stack shape in pop"

  let rec show to_string s =
    S.to_list s >>= fun l ->
    return (String.concat "::" (List.map to_string l))

end

module MQUEUE_Make (S: Irmin_heap.S)
= struct

  type t = S.t
  type elt = S.elt
  module Path = S.Path
  let merge = S.merge

  let create () =
    S.create () >>= fun empty1 ->
    S.create () >>= fun empty2 ->
    S.build None [empty1; empty2]
  
  let push s elt =
    S.read_exn s >>= function
    | None, [pushed; to_pop] ->
      S.build (Some elt) [pushed] >>= fun pushed ->
      S.create () >>= fun s -> 
      S.build None [pushed; to_pop]
    | _ -> failwith "incorrect queue shape"
  
  let pop s =
    S.read_exn s >>= function
    | None, [pushed;to_pop] ->
      begin
        S.read_exn to_pop >>= function
        | Some elt, [to_pop] ->
          S.build None [pushed;to_pop] >>= fun s ->
          return (Some elt, s)
        | None, [] ->
          begin
            S.to_list pushed >>= fun l ->
            match (List.rev l) with
            | [] -> return (None, s)
            | x::xs ->
              S.of_list xs >>= fun to_pop ->
              S.create () >>= fun pushed ->
              S.build None [pushed; to_pop] >>= fun s ->
              return (Some x, s)
          end
        | _ -> failwith "incorrect queue shape"
      end
    | _ -> failwith "incorrect queue shape"

  let show to_string s = 
    S.read_exn s >>= function
    | None, [pushed;to_pop] ->
      S.to_list pushed >>= fun pushed ->
      S.to_list to_pop >>= fun to_pop ->
      return ((String.concat ":" (List.map to_string pushed))^"//"^
              (String.concat ":" (List.map to_string to_pop)))
    | _ -> failwith "incorrect queue shape in show"
  
end

module type MTREE = sig
  type t
  type elt
  val build : elt option -> t list -> t Lwt.t
  val destr : t -> (elt option * t list) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
  module Path : Irmin.Path.S
  val merge : Path.t -> t option Irmin.Merge.t
end

module MTREE_Make (S: Irmin_heap.S)
= struct

  type t = S.t
  type elt = S.elt
  module Path = S.Path
  let merge = S.merge

  let build = S.build 

  let destr = S.read_exn

  let rec show to_string s =
    destr s >>= fun (elt, l) ->
    let str_elt = match elt with
      | None -> "None"
      | Some e -> to_string e in
    let rec f = function
      | [] -> return ""
      | x::xs -> show to_string x >>= fun str ->
        f xs >>= fun str2 -> return ("("^str^")"^str2) in
    f l >>= fun str ->
    return (str_elt^":"^str)

end
