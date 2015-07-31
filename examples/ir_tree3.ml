open Lwt
open Irmin_unix    
              
module Git = Irmin_git.AO(Git.Memory)
module Config2 = struct
  let conf = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
  let task = Irmin_unix.task
end
module Path = Irmin.Path.String_list
module S = Irmin_heap.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config2)
module Tree = Irmin_datatypes.MTREE_Make(S)

let store = Irmin.basic (module Irmin_git.FS) (module Tree)
let config = Config2.conf

let key : Tree.Path.t = [ "foo" ]

let read t =
  Irmin.read (t "reading") key >>= function
  | None -> failwith "empty read"
  | Some q -> Lwt.return q

let build t v l =
  read t >>= fun q ->
  Tree.build (Some v) l >>= fun q ->
  Irmin.update (t "build") key q

let destr t =
  read t >>= fun q ->
  Tree.destr q

let print t =
  read t >>= fun q ->
  Tree.show string_of_int q >>= fun s ->
  print_endline s; return ()

type tree = Leaf of int | Node of int * (tree list)

let build_of_tree t tree =
  let rec f = function
    | Leaf n -> Tree.build (Some n) []
    | Node (n,l) ->
      Lwt_list.map_s f l >>= fun l ->
      Tree.build (Some n) l in
  f tree >>= fun tree ->
  Irmin.update (t "build of tree") key tree

let test_main old s1 s2 =
  Config.init () ;
  Irmin.create store config task >>= fun t ->
  
  build_of_tree t old >>= fun () ->

  print t >>= fun () ->
  Irmin.clone_force task (t "cloning") "test" >>= fun x ->
  build_of_tree t s1 >>= fun () ->
  build_of_tree x s2 >>= fun () ->
  
  print t >>= fun () ->
  print x >>= fun () ->
  
  Irmin.merge_exn "merging x into t" t ~into:x >>= fun () ->

  print x >>= fun () ->

  Lwt.return ()

let main1 () =
  let old = Leaf 1 in
  let s1 = Node (2, [Leaf 3; Leaf 4]) in
  let s2 = Node (5, [Leaf 6; Leaf 7; Leaf 8]) in
  test_main old s1 s2

let main2 () =
  let old = Node (2, [Leaf 3; Leaf 4]) in
  let s1 = Node (2, [Node(5,[Leaf 7]); Leaf 4]) in
  let s2 = Node (2, [Leaf 3; Leaf 6]) in
  test_main old s1 s2

let main3 () =
  let old = Node (1, [Leaf 2; Node (3, [Leaf 4;Leaf 5])]) in
  let s1 = Node (1, [Leaf 8; Node (3, [Leaf 4;Leaf 5])]) in
  let s2 = Node (1, [Node (6, [Node (7, [Leaf 2])]); Leaf 5]) in
  test_main old s1 s2
  
let () =
  main3 () |> Lwt_unix.run
