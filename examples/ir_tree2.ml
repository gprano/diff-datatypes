let (>>=) = Lwt.bind

module Git = Irmin_git.AO(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end
module Path = Irmin.Path.String_list
module S = Irmin_heap.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)
module Tree = Irmin_datatypes.MTREE_Make(S)

let get_val =
  let c = ref (-1) in
  (fun () -> incr c; !c)

let main() =
  Tree.build (Some (get_val ())) [] >>= fun t0 ->
  Tree.build (Some (get_val ())) [t0] >>= fun t1 ->

  Tree.destr t1 >>= function
  | (Some e, l) ->
    begin
    Tree.build (Some (get_val ())) [] >>= fun t2 ->
    Tree.build (Some e) [t2] >>= fun t3 ->
    Tree.build (Some (get_val ())) [t3] >>= fun t4 ->

    let old = Irmin.Merge.promise (Some t1) in
    Tree.merge [] ~old (Some t3) (Some t4) >>= fun res ->
    match res with
    | `Conflict s  -> failwith s
    | `Ok None     -> failwith "none"
    | `Ok (Some res) ->
      Tree.show string_of_int res >>= fun res ->
      Tree.show string_of_int t1 >>= fun t1 ->
      Tree.show string_of_int t3 >>= fun t3 ->
      Tree.show string_of_int t4 >>= fun t4 ->
      List.iter print_endline [t1;t3;t4;res];
      Lwt.return ()
    end
  | _ -> failwith "bug"


let () =
  Lwt_unix.run (main ())
