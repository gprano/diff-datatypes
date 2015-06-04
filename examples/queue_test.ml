open Datatypes
open Printf

let q1 = IQueue.empty
let () = Heap.show_obj q1
let q2 = IQueue.push q1 0
let () = Heap.show_obj q2
let es = Heap.diff q1 q2
let () = print_string @@ Heap.string_of_edit_script es
let () = print_string "\n\n"

let () = Heap.show_obj (ref [])
let () = Heap.show_obj (ref [1])
let () = Heap.show_obj (ref [2])
let () = print_string "This -->"
let () = print_string @@
         Heap.string_of_edit_script @@
         Heap.diff [] [1]
let () = print_string "\n\n"

open Tree

let () = printf "----------\n"
