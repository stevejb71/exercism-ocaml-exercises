open Core.Std

type nucleotide = A | C | G | T

let zip_with (xs: 'a list) (ys: 'b list) ~(f: 'a -> 'b -> 'c): 'c list =
  let rec stack_safe acc xs ys = match (xs, ys) with
    | ([], _) -> acc
    | (_, []) -> acc
    | (x::xs, y::ys) -> stack_safe ((f x y)::acc) xs ys
  in stack_safe [] xs ys

let hamming_distance s1 s2 =
  List.count (zip_with s1 s2 ~f:(=)) ~f:((=) false)
