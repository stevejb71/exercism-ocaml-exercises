open Core.Std

let to_int =
  let to_hex ch r = match Char.uppercase ch with
    | '0'..'9' -> Char.to_int ch - Char.to_int '0'
    | 'A'..'F' -> Char.to_int ch - Char.to_int 'a' + 10
    | _ -> r.return 0 in
  String.fold ~init:0 ~f:(fun acc ch -> with_return (fun r -> acc * 16 + (to_hex ch r)))
