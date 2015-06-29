let (>>=) = Lwt.bind

module Git = Irmin_git.AO(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end
module Path = Irmin.Path.String_list
module S = Irmin_heap.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)
module Stack = Irmin_datatypes.MSTACK_Make(S)

let get_val =
  let c = ref (-1) in
  (fun () -> incr c; !c)

let main () =
  Stack.create () >>= fun q0 ->
  
  (** the stuff to do on q0 *)
  Stack.push q0 (get_val ()) >>= fun q0 -> 

  (** branching to q1,q2 *)
  Lwt.return q0                      >>= fun q1 ->
  Lwt.return q0                      >>= fun q2 ->

  (** the stuff to do on q1 *)
  Stack.pop q1                       >>= fun (_,q1) ->
  Stack.push q1 (get_val ())         >>= fun q1 ->
  Stack.push q1 (get_val ())         >>= fun q1 ->
  (* Stack.pop q1                    >>= fun (_,q1) -> *)
  (* Stack.pop q1                    >>= fun (_,q1) -> *)

  (** the stuff to do on q2 *)
  Stack.push q2 (get_val ())            >>= fun q2 ->
  Stack.push q2 (get_val ())            >>= fun q2 ->
  (*Stack.pop q2                   >>= fun (_,q2) ->*)
  (*Stack.pop q2                   >>= fun (_,q2) ->*)

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
