open Core.Std

let increment (b: char array) (index: int): unit =
  let increment_char = function
    | ' ' -> '1'
    | '*' -> '*'
    | ch -> Char.of_int_exn (49 + Char.get_digit_exn ch) in
  Array.set b index (Array.get b index |> increment_char)

let for_each_neighbour (width: int) (length: int) (index: int) ~(f: int -> unit): unit =
  let (x, y) = (index mod width, index / width)
  in let in_bounds x y = x >= 0 && y >= 0 && x < width && y < length
  in
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      if dx <> 0 || dy <> 0
      then
        let x' = x + dx and y' = y + dy in
        if in_bounds x' y' then f (x' + y' * width)
    done;
  done;;

let for_all_mines (b: char array) (f: int -> unit): unit =
  for index = 0 to (Array.length b - 1) do
    if Array.get b index = '*'
    then f index;
  done;;

let to_array (b: string list): char array =
  List.map b (String.to_list) |> List.join |> Array.of_list

let rec split_list (n: int) (xs: 'a list): ('a list) list = match xs with
  | [] -> []
  | _ ->
    let (hd, tl) = List.split_n xs n in
    hd :: (split_list n tl)

let to_list (width: int) (b: char array): string list =
  Array.to_list b |> split_list width |> List.map ~f:(String.of_char_list)

let annotate b =
  if List.is_empty b then []
  else
    let b_array = to_array b
    in let length = List.length b
    in let width = String.length (List.hd_exn b)
    in let increment_counts = increment b_array
    in
      for_all_mines b_array (for_each_neighbour width length ~f:increment_counts);
      to_list width b_array
