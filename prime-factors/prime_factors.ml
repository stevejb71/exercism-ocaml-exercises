open Core.Std
open Int64
module S = Sequence

let isqrt (n: int64): int64 = n |! Big_int.big_int_of_int64 |> Big_int.sqrt_big_int |> Big_int.int64_of_big_int

(** Returns a sequence of the form [a; f a; f f a;...]. *)
let repeatedly (init: 'a) (f: 'a -> 'a): 'a Sequence.t =
  S.unfold ~init ~f:(fun s -> Some (s, f s))

let possible_proper_divisors (n: int64): int64 Sequence.t =
  let max_divisor = isqrt n in
  S.of_list [
    S.of_list [2L; 3L];
    repeatedly 1L ((+) 1L) |> S.concat_map ~f:(fun x -> S.of_list [6L*x - 1L; 6L*x + 1L]);
  ] |> S.concat |> S.take_while ~f:(fun n -> n <= max_divisor)

let first_proper_divisor (n: int64): int64 option =
  possible_proper_divisors n |> S.find ~f:(fun f -> n % f = 0L)

let rec find_factors_of (n: int64): int64 list =
  match first_proper_divisor n with
    | None -> [n]
    | Some d -> d :: (find_factors_of (n / d))

let factors_of = function
  | 1L -> []
  | n -> find_factors_of n |> List.sort ~cmp:Int64.compare
