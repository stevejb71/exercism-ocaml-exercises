open Core.Std

type 'a crumb = L of ('a * ('a Tree.t) option) | R of ('a * ('a Tree.t) option) [@@deriving sexp]
type 'a t = {tree: 'a Tree.t; crumbs: ('a crumb) list} [@@deriving sexp]

let equal f a b = false

let of_tree t = failwith "undefined"

let to_tree z = failwith "undefined"

let value z = failwith "undefined"

let left z = None

let right z = None

let up z = None

let set_value v z = z

let set_left ot z = z

let set_right ot z = z
