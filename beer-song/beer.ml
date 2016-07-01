open Core.Std
open Printf

let bottles = function
  | 0 -> "no more bottles"
  | 1 -> "1 bottle"
  | n -> sprintf "%d bottles" n

let one_or_it = function
  | 1 -> "it"
  | _ -> "one"

let verse = function
  | 0 -> "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
  | n ->
      let n_bottles = bottles n and n_minus_1_bottles = bottles (n - 1) and one = one_or_it n in
      sprintf "%s of beer on the wall, %s of beer.\nTake %s down and pass it around, %s of beer on the wall.\n" n_bottles n_bottles one n_minus_1_bottles

let sing ~from ~until =
  List.range from (until - 1) ~stride:(-1)
  |> List.map ~f:verse
  |> String.concat ~sep:"\n"
