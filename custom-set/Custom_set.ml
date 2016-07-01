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

module Make(S: Spec) = struct
  type elt_type = S.t
  type t = S.t list

  let empty = []

  let equal =
    let rec stack_safe acc xs ys =
      acc && match (xs, ys) with
      | ([], _) -> List.is_empty ys
      | (_, []) -> List.is_empty xs
      | (x::xs, y::ys) -> stack_safe (S.equal x y) xs ys
    in stack_safe true

  let compare xs ys = 0

  let rec to_list xs = xs

  let to_string xs =
    let contents = List.map xs S.to_string |> String.concat ~sep:" "
    in "{" ^ contents ^ "}"

  let add xs n =
    let rec stack_safe acc xs = match xs with
      | [] -> n::acc
      | hd::tl -> match S.compare n hd with
        | 0 -> xs
        | r when r > 0 -> (hd::n::tl) @ acc
        | _ -> stack_safe (hd::acc) tl
    in stack_safe [] xs |> List.rev

  let of_list xs = List.fold ~init:empty ~f:add xs |> List.rev

  let difference xs ys =
    List.filter (to_list xs) ~f:(fun x -> (List.mem ~equal:(S.equal) ys x |> not)) |> of_list

  let remove xs n = List.filter xs ~f:((<>) n)

  let intersect xs ys = difference xs (difference xs ys)

  let union xs ys =
    List.fold (to_list ys) ~init:xs ~f:(fun acc y -> add acc y)
end

module I = Make(IntSpec)
let x3 = I.add (I.empty) 3
let x33 = I.add x3 3
let x34 = I.add x3 4
let x344 = I.add x34 4
