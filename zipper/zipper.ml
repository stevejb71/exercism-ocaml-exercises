open Core.Std

type 'a t =
  Top of 'a Tree.t |
  Left of ('a Tree.t * 'a t) |
  Right of ('a Tree.t * 'a t) [@@deriving sexp]

let tree_equal f a b = let open Tree in
  f a.value b.value  &&
  Option.equal (Tree.equal f) a.left b.left &&
  Option.equal (Tree.equal f) a.right b.right

let rec equal f a b = match (a, b) with
  | (Top t1, Top t2) -> tree_equal f t1 t2
  | (Left (t1, z1), Left (t2, z2)) -> tree_equal f t1 t2 && equal f z1 z2
  | (Right (t1, z1), Right (t2, z2)) -> tree_equal f t1 t2 && equal f z1 z2
  | _ -> false

let of_tree tr = Top tr

let parent = function
  | Top _ -> failwith "parent called on Top"
  | Left (t, p) -> p
  | Right (t, p) -> p

let focus = function
  | Top t -> t
  | Left (t, p) -> t
  | Right (t, p) -> t

let rec to_tree = function
  | Top t -> t
  | z -> parent z |> to_tree

let value z = (focus z).Tree.value

let left z = (focus z).Tree.left |> Option.map ~f:(fun x -> Left (x, z))

let right z = (focus z).Tree.right |> Option.map ~f:(fun x -> Right (x, z))

let up = function
  | Top _ -> None
  | z -> Some (parent z)

let rec modify ~(f: 'a Tree.t -> 'a Tree.t) (z: 'a t): 'a t =
  let t = f (focus z) in match z with
    | Top _ -> Top t
    | Left _ -> set_left (Some t) (parent z) |> left |> Option.value_exn
    | Right _ -> set_right (Some t) (parent z) |> right |> Option.value_exn
and set_value v = modify ~f:(fun t -> {t with Tree.value = v})
and set_left ot = modify ~f:(fun t -> {t with Tree.left = ot})
and set_right ot = modify ~f:(fun t -> {t with Tree.right = ot})
