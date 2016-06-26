open Core.Std

module type Spec = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val to_string: t -> string
end

module IntSpec = struct
  type t = int
  let compare = Int.compare
  let equal n m = n = m
  let to_string = Int.to_string
end

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

module Make(S: Spec) = struct
  type elt_type = S.t
  type t = S.t tree

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec equal xs ys = match (xs, ys) with
    | (Empty, Empty) -> true
    | (Empty, Node(_, _, _)) -> false
    | (Node(_, _, _), Empty) -> false
    | (Node (l1, x1, r1), Node (l2, x2, r2))
      -> S.equal x1 x2 && equal l1 l2 && equal r1 r2
    (* | _ -> false *)

  let compare xs ys = match (xs, ys) with
    | (Empty, Empty) -> 0
    | (Empty, Node(_, _, _)) -> 1
    | (Node(_, _, _), Empty) -> (-1)
    | (Node (xl, x, xr), Node (yl, y, yr)) -> match S.compare x y with
      | n when n < 0 -> -1
      | n when n > 0 -> 1
      | 0 -> match compare xl yl with
        | n when n < 0 -> compare xr yr
        | 0 -> 0
        | n when n > 0 -> 1

  let rec to_list xs = match xs with
    | Empty -> []
    | Node (l, x, r) -> to_list l @ (x :: (to_list r))

  let to_string xs =
    let contents = List.map (to_list xs) S.to_string |> String.concat ~sep:" "
    in "{" ^ contents ^ "}"

  let rec add xs n = match xs with
    | Empty -> Node (Empty, n, Empty)
    | Node (l, m, r) -> match S.compare m n with
      | x when x > 0 -> Node ((add l n), m, r)
      | x when x < 0 -> Node (l, m, (add r n))
      | 0 -> Node (l, m, r)

  let of_list xs = List.fold ~init:empty ~f:add xs

  let rec member (xs: t) (n: elt_type): bool = match xs with
    | Empty -> false
    | Node (l, x, r) -> match S.compare n x with
      | x when x < 0 -> member l n
      | x when x > 0 -> member r n
      | 0 -> true

  let difference xs ys =
    List.filter (to_list xs) ~f:(fun x -> (member ys x |> not)) |> of_list

  let remove xs n = difference xs (Node (Empty, n, Empty))

  let intersect xs ys = difference xs (difference xs ys)

  let union xs ys =
    List.fold (to_list ys) ~init:xs ~f:(fun acc y -> add acc y)
end
