let (>>=) = Lwt.bind

module Git = Irmin_git.AO(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end
module Path = Irmin.Path.String_list
module S = Irmin_heap.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)
module Stack = Irmin_datatypes.MQUEUE_Make(S)

let get_val =
  let c = ref (-1) in
  (fun () -> incr c; !c)

let rec push_n s n =
  if n = 0 then Lwt.return s
  else
    Stack.push s (get_val ()) >>= fun s ->
    push_n s (n-1)

let rec pop_n s n =
  if n = 0 then Lwt.return s
  else
    Stack.pop s >>= fun (_,s) ->
    pop_n s (n-1)

let main () =
  Stack.create () >>= fun q0 ->

  (** the stuff to do on q0 *)
  push_n q0 300 >>= fun q0 ->

  (** branching to q1,q2 *)
  Lwt.return q0                      >>= fun q1 ->
  Lwt.return q0                      >>= fun q2 ->

  (** the stuff to do on q1 *)
  pop_n q1 23 >>= fun q1 ->
  push_n q1 100 >>= fun q1 ->

  (** the stuff to do on q2 *)
  pop_n q2 50 >>= fun q2 ->
  push_n q2 100 >>= fun q2 ->

  (** the merging part *)
  let old = Irmin.Merge.promise (Some q0) in
  Stack.merge [] ~old (Some q1) (Some q2) >>= fun res ->
  match res with
  | `Conflict s  -> failwith s
  | `Ok None     -> failwith "none"
  | `Ok (Some res) -> (
      (** writing the result *)
      Stack.show string_of_int res >>= fun res ->
      Stack.show string_of_int q0 >>= fun q0 ->
      Stack.show string_of_int q1 >>= fun q1 ->
      Stack.show string_of_int q2  >>= fun q2 ->
      List.iter print_endline [q0;q1;q2;res];
      Lwt.return () )

let () =
  Lwt_unix.run (main ())
