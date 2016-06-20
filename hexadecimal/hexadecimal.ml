open Core.Std

let letter_to_hex ch = match ch with
  | '0'..'9' -> Some (Char.to_int ch - 48)
  | 'a'..'f' -> Some (Char.to_int ch - 87)
  | 'A'..'F' -> Some (Char.to_int ch - 55)
  | _ -> None

let convert_digit acc ch =
  Option.map2 acc (letter_to_hex ch) (fun acc d -> acc * 16 + d)

let to_int s =
  let maybe_hex = String.fold ~init:(Some 0) ~f:convert_digit s in
  Option.value ~default:0 maybe_hex
