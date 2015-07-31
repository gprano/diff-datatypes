open Lwt
open Irmin_unix    
              
module Git = Irmin_git.AO(Git.Memory)
module Config2 = struct
  let conf = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
  let task = Irmin_unix.task
end
module Path = Irmin.Path.String_list
module S = Irmin_heap.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config2)
module Stack = Irmin_datatypes.MSTACK_Make(S)

let store = Irmin.basic (module Irmin_git.FS) (module Stack)
let config = Config2.conf


let key : Stack.Path.t = [ "foo" ]

let read t =
  Irmin.read (t "reading") key >>= function
  | None -> failwith "empty read"
  | Some q -> Lwt.return q

let push t v =
  read t >>= fun q ->
  Stack.push q v >>= fun q ->
  Irmin.update (t "pushing element") key q

let empty t =
  Stack.create () >>= fun q ->
  Irmin.update (t "empty") key q

let print t =
  read t >>= fun q ->
  Stack.show string_of_int q >>= fun s ->
  print_endline s; return ()

let pop t =
  read t >>= fun q ->
  Stack.pop q >>= fun (_,q) ->
  Irmin.update (t "pop") key q

let main () =
  Config.init () ;
  Irmin.create store config task >>= fun t ->
  empty t >>= fun () ->
  print_endline "-";
  Irmin.clone_force task (t "cloning") "test" >>= fun x ->
  print_endline "-";
  push t 1 >>= fun () ->
  print_endline "-";
  push x 2 >>= fun () ->
  print_endline "-";
  Irmin.clone_force task (t "cloning") "test2" >>= fun t2 ->
  print_endline "-";
  Irmin.merge_exn "Merging x into t" x ~into:t >>= fun () ->
  print_endline "-";
  Irmin.merge_exn "Merging x into t" t2 ~into:x >>= fun () ->
  print_endline "-";
  print t >>= fun () ->
  print_endline "-";
  print x >>= fun () ->
  print_endline "-";
  push x 3 >>= fun () ->
  push x 3 >>= fun () ->
  print_endline "-";
  Irmin.merge_exn "" x ~into:t >>= fun () ->
  print_endline "-";
  print t >>= fun () ->
  print_endline "-";
  pop t >>= fun () -> pop t >>= fun () ->
  print_endline "-";
  pop x >>= fun () -> pop x >>= fun () ->
  print_endline "-";
  Irmin.merge_exn "merging x into t" x ~into:t >>= fun () ->
  print_endline "-";
  print t >>= fun () ->
  print_endline "-";
  Lwt.return ()

let () =
  main () |> Lwt_unix.run
