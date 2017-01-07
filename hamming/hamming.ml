open Core.Std

type nucleotide = A | C | G | T

let hamming_distance ns1 ns2 =
  List.fold2
            ~init:0
            ~f:(fun acc x y -> if x = y then acc else acc + 1) ns1 ns2