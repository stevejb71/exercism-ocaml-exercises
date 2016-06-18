open Core.Std

let neg p x = p x |> not

let is_shouting =
  String.for_all ~f:(neg Char.is_lowercase)

let is_question str = str.[String.length str - 1] = '?'

let is_silence str = String.strip str |> String.is_empty

let is_only_nonalpha =
  String.for_all ~f:(neg Char.is_alpha)

let response_for speech =
  if is_silence speech then "Fine. Be that way!"
  else if is_question speech then
    if String.drop_suffix speech 1
       |> String.for_all ~f:(fun ch -> Char.is_uppercase ch || ch = ' ')
    then "Whoa, chill out!"
    else "Sure."
  else if is_only_nonalpha speech then "Whatever."
  else if is_shouting speech then "Whoa, chill out!"
  else "Whatever."
