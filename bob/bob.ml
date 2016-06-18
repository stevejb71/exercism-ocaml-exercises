open Core.Std

let is_shouting str =
  String.for_all ~f:(fun c -> Char.is_lowercase c |> not) str

let is_question str = str.[String.length str - 1] = '?'

let is_silence str = String.strip str |> String.is_empty

let is_only_nonalpha str =
  String.for_all ~f:(fun c -> (Char.is_alpha c |> not)) str

let response_for speech =
  if is_silence speech then "Fine. Be that way!"
  else if is_question speech then
    if String.drop_suffix speech 1 |> String.for_all ~f:(fun ch -> Char.is_uppercase ch || ch = ' ')
    then "Whoa, chill out!"
    else "Sure."
  else if is_only_nonalpha speech then "Whatever."
  else if is_shouting speech then "Whoa, chill out!"
  else "Whatever."
