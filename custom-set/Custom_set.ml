open Core.Std

module type ELEMENT = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end

type 'a node = {
  value: 'a;
  left: 'a bst;
  right: 'a bst;
}
and 'a bst = Empty | Node of 'a node

let singleton x = Node {value = x; left = Empty; right = Empty}

let (<<) f g x = f (g x)

module Make(El: ELEMENT) = struct
  type el = El.t
  type t = el bst

  let (<) (l: el) (r: el): bool = El.compare l r < 0
  let (=) = El.equal

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec is_member x n = match x with
    | Node {value = v; _} when v = n -> true
    | Node {left = l; value = v; _} when n < v -> is_member l n
    | Node {right = r; _} -> is_member r n
    | _ -> false

  let rec equal a b = match (a, b) with
    | (Empty, Empty) -> true
    | (Node {left = l1; value = v1; right = r1}, Node {left = l2; value = v2; right = r2}) ->
        v1 = v2 && equal l1 l2 && equal r1 r2
    | _ -> false

  let rec add x n = match x with
    | Empty -> singleton n
    | Node {value = v; _} when n = v -> x
    | Node {left = l; value = v; right = r} when n < v ->
        Node {left = add l n; right = r; value = v}
    | Node {left = l; value = v; right = r} ->
        Node {left = l; right = add r n; value = v}

  let rec to_list = function
    | Empty -> []
    | Node {value = v; left = l; right = r} -> to_list l @ (v :: to_list r)

  let of_list_from init = List.fold ~init ~f:add

  let union x = of_list_from x << to_list

  let of_list = of_list_from empty

  let to_string x =
    let l = List.map ~f:El.to_string (to_list x) in
    sprintf "{%s}" (String.concat ~sep:" " l)

  let rec remove x n = match x with
    | Empty -> Empty
    | Node {left = l; value = v; right = r} when n = v -> (
        match (l, r) with
          | (Empty, _) -> r
          | (_, Empty) -> l
          | (Node {left = ll; value = vl; right = rl}, _) -> Node {left = remove ll n; value = vl; right = rl}
        )
    | Node {left = l; value = v; right = r} when n < v -> Node {left = remove l n; value = v; right = r}
    | Node {left = l; value = v; right = r} -> Node {left = l; value = v; right = remove r n}

  let difference x = List.fold ~init:x ~f:remove << to_list

  let intersect xs = List.fold ~init:empty ~f:(fun acc y -> if is_member xs y then add acc y else acc) << to_list
end
