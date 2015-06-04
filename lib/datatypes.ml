module type IQUEUE = sig
  type 'a t
  val empty : 'a t
  val push  : 'a t -> 'a -> 'a t
  val pop   : 'a t -> 'a option * 'a t
end

module IStack : IQUEUE = struct
  type 'a t = 'a list

  let empty = []

  let push s v = v::s

  let pop = function
    | [] -> (None, [])
    | x::xs -> (Some x,xs)
end

module IQueue : IQUEUE = struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let push (front, back) v = (front, v::back)

  let pop (front, back) =
    match front with
    | [] -> (match List.rev back with
             | [] -> (None, ([],[]))
             | x::xs -> (Some x, (xs, [])))
    | x::xs -> (Some x, (xs, back))
end

module Tree = struct
  type 'a t = Leaf | Node of 'a t * 'a * 'a t

  let rec show to_str = function
  | Leaf -> "Leaf"
  | Node (l,v,r) ->
      String.concat "" ["("; show to_str l; ",";
                        to_str v;
                        ","; show to_str r; ")"]
end

