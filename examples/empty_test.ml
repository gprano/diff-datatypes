open Datatypes
open Printf

let s1 = IStack.empty
let s1 = IStack.push s1 3
let () = printf "Stack with one element:\n"
let () = Heap.show_obj s1
let (_,s2) = IStack.pop s1
let () = printf "After a pop():\n"
let () = Heap.show_obj s2    
let es = Heap.diff s1 s2
let () = printf "Edit script:\n%s\n" (Heap.string_of_edit_script es)

let () = printf "\n----------\n\n"

let s1 = IQueue.empty
let s1 = IQueue.push s1 3
let () = printf "Queue with one element:\n"
let () = Heap.show_obj s1
let (_,s2) = IQueue.pop s1
let () = printf "After a pop():\n"
let () = Heap.show_obj s2    
let es = Heap.diff s1 s2
let () = printf "Edit script:\n%s\n" (Heap.string_of_edit_script es)

let () = printf "\n----------\n\n"

let s1 = Tree.Node (Tree.Leaf, 3, Tree.Leaf)
let () = printf "Tree with one element:\n"
let () = Heap.show_obj s1
let s2 = Tree.Leaf
let () = printf "Leaf:\n"
let () = Heap.show_obj s2
let es = Heap.diff s1 s2
let () = printf "Edit script:\n%s\n" (Heap.string_of_edit_script es)
