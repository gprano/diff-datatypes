open Datatypes
open Printf

let s1 = IStack.empty
let () = Heap.show_obj s1           
let s2 = IStack.push s1 3
(*let s2 = IStack.push s2 4*)
let () = Heap.show_obj s2
let (_,s3) = IStack.pop s2
let s4 = IStack.push s2 6
let () = Heap.show_obj s3
let () = Heap.show_obj s4
let es0 = Heap.diff s2 s2
let () = print_string @@ Heap.string_of_edit_script es0
let () = print_string "\n\n"
let es1 = Heap.diff s2 s3
let () = print_string @@ Heap.string_of_edit_script es1
let () = print_string "\n\n"
let es2 = Heap.diff s2 s4
let () = print_string @@ Heap.string_of_edit_script es2
let () = print_string "\n\n"
let es3 = Heap.merge es1 es2
let () = print_string @@ Heap.string_of_edit_script es3
let () = print_string "\n\n"
let s5 = Heap.patch s2 es3
let () = Heap.show_obj s5
    
let () = print_string "\n---------------------\n\n"
