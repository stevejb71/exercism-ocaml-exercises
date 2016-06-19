open Core.Std

let for_each_letter lc w inc =
  for i = 0 to String.length w - 1 do
    let index = Char.to_int w.[i] - 97 in
    lc.(index) <- lc.(index) + inc;
  done;
  ()

let make_letter_counts_array w =
  let letter_counts = Array.create ~len:26 0 in
  for_each_letter letter_counts w 1;
  letter_counts

let is_anagram letter_counts w: bool =
  let letter_counts = Array.copy letter_counts in
  for_each_letter letter_counts w (-1);
  Array.for_all letter_counts ~f:((=) 0)

let anagrams word candidates =
  let word = String.lowercase word in
  let letter_counts = make_letter_counts_array word in
  let is_anagram_and_not_eq c =
    let c = String.lowercase c in
    c <> word && is_anagram letter_counts c in
  List.filter candidates ~f:is_anagram_and_not_eq
