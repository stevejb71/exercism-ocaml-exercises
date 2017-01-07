open Core.Std

let encode_char ch =
  if Char.is_alpha ch
  then Char.to_int 'z' - (Char.to_int (Char.lowercase ch) - Char.to_int 'a') |> Char.of_int_exn
  else ch

let (>>>) f g x = g (f x)

let decode =  
  String.filter ~f:(Char.is_alphanum)
  >>> String.map ~f:encode_char

let encode ?block_size:(block_size = 5) =
  decode
  >>> String.to_list
  >>> List.groupi ~break:(fun i _ _ -> i % block_size = 0)
  >>> List.map ~f:String.of_char_list
  >>> String.concat ~sep:" "