open Core.Std

module SMap = String.Map

let neg f x = f x |> not

let add_to_map wcs w =
  SMap.update wcs w ~f:(fun k -> Option.value_map k ~default:1 ~f:((+) 1))

let normalize = function
  | ch when Char.is_alphanum ch -> Char.lowercase ch
  | _ -> ' '

let word_count s =
  let s = String.map s ~f:normalize in
  let split = List.filter (String.split s ~on:' ') ~f:(neg String.is_empty) in
  List.fold ~init:SMap.empty ~f:add_to_map split
