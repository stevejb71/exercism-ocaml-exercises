open Core.Std

let (<<) f g x = f (g x)

let every_second_digit_doubled =
  let double_digit n = let d = n * 2 in if d >= 10 then d - 9 else d in
  List.rev_mapi ~f:(fun i -> if i % 2 = 1 then double_digit else Fn.id) << List.rev

let to_int ch = Char.to_int ch - Char.to_int '0'

let sum (init: 'a) (add: 'a -> 'a -> 'a) = List.fold ~init ~f:add

let valid s =
  let ds = String.to_list s |> List.map ~f:to_int in
  let checksum = every_second_digit_doubled ds |> sum 0 (+)
  in checksum % 10 = 0
