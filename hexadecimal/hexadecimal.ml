open Core.Std

let to_int s =
  with_return(fun r ->
    let to_hex ch = let ch = Char.lowercase ch in match ch with
      | '0'..'9' -> Char.to_int ch - Char.to_int '0'
      | 'a'..'f' -> Char.to_int ch - Char.to_int 'a' + 10
      | _ -> r.return 0 in
    String.fold ~init:0 ~f:(fun acc ch -> acc * 16 + to_hex ch) s
  )
