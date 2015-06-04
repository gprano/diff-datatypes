(** Heap module *)

type edit_script
  
val string_of_edit_script : edit_script -> string
  
val diff : 'a -> 'a -> edit_script
  
val patch : 'a -> edit_script -> 'a
  
type 'b action =
  {on_entry : 'b -> Obj.t -> 'b;
   on_exit  : 'b -> Obj.t -> 'b;
   on_cross : 'b -> Obj.t -> 'b}
  
val dfs : Obj.t -> 'b action -> 'b -> 'b
  
val show_obj : 'a -> unit
  

    
