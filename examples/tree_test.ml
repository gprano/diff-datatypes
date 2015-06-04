open Printf
open Datatypes.Tree

let t1 = Node (Node (Leaf, 1, Leaf) , 0, Node (Leaf, 2, Leaf))
let t2 = Node (Node (Leaf, 1, Leaf) , 0, Node (Node (Leaf, 3, Leaf), 2, Node (Leaf, 4, Leaf)))
let es = Heap.diff t1 t2
let () = print_string @@ Heap.string_of_edit_script es
let () = print_string "\n\n"
let t3 = Heap.patch t1 es
let () = print_string @@ show string_of_int t3
let () = print_string "\n"

let l1 = []
let l2 = [1]
let es = Heap.diff l1 l2
let () = print_string @@ Heap.string_of_edit_script es
let () = print_string "\n"
let l3 = Heap.patch l1 es
let () = List.iter (fun i -> printf "%d," i) l3
let () = print_string "\n"
