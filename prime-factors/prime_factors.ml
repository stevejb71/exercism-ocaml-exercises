open Batteries
open Core.Std
open Int64
module BL = Batteries.List

let isqrt (n: int64): int64 = n |! to_float |> sqrt |> of_float

let possible_divisors (n: int64): int64 BatEnum.t =
  let max_divisor = isqrt n
  in BL.enum [
    Enum.singleton 2L;
    Enum.singleton 3L;
    Enum.seq 5L ((+) 6L) ((>=) max_divisor);
    Enum.seq 7L ((+) 6L) ((>=) max_divisor);
    Enum.singleton n;
    ] |> Enum.flatten

let find_first_divisor (n: int64): int64 =
  Enum.find (fun f -> rem n f = 0L) (possible_divisors n)

let rec find_factors_of (n: int64): int64 list =
  let first_divisor = find_first_divisor n
  in
    if first_divisor = n
    then [n]
    else first_divisor :: (find_factors_of (n / first_divisor))

let factors_of = function
  | 1L -> []
  | n -> find_factors_of n |> List.sort ~cmp:Int64.compare
