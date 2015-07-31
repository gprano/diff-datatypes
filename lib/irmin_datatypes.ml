open Lwt

module type MSTACK = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop : t -> (elt option * t) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
end

module MSTACK_Make (S: Irmin_heap.S)
= struct

  include S
                
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

  include S

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

  let normalize s = 
    S.read_exn s >>= function
    | None, [pushed; to_pop] ->
      S.to_list pushed >>= fun pushed_l ->
      S.to_list to_pop >>= fun to_pop_l ->
      S.of_list (to_pop_l @ List.rev pushed_l) >>= fun to_pop ->
      S.create () >>= fun pushed ->
      S.build None [pushed;to_pop]      
    | _ -> failwith "incorrect shape"
  
  let rec pop s =
    S.read_exn s >>= function
    | None, [pushed;to_pop] ->
      begin
        S.read_exn to_pop >>= function
        | Some elt, [to_pop] ->
          S.build None [pushed;to_pop] >>= fun s ->
          return (Some elt, s)
        | None, [] ->
          begin
            S.read_exn pushed >>= function
            | None, [] -> return (None, s)
            | Some _, [_] ->
              normalize s >>= fun s -> pop s
            | _ -> failwith "incorrect queue shape"
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

  let merge =
    let merge3 ~old s1 s2 =
      old () >>= function
      | `Conflict _ | `Ok None -> Irmin.Merge.OP.conflict "merge"
      | `Ok (Some old) ->
        normalize old >>= fun old ->
        normalize s1 >>= fun s1 ->
        normalize s2 >>= fun s2 ->
        S.merge3 ~old:(fun () -> return (`Ok (Some old))) s1 s2 in
    fun _path -> Irmin.Merge.option (module S) merge3
  
end

module type MTREE = sig
  include Irmin.Contents.S
  type elt
  val build : elt option -> t list -> t Lwt.t
  val destr : t -> (elt option * t list) Lwt.t
  val show : (elt -> string) -> t -> string Lwt.t
end

module MTREE_Make (S: Irmin_heap.S)
= struct

  include S

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

