open Core.Std

let (<<) = Fn.compose

let number n =
  let digits = String.filter ~f:Char.is_digit n
  in match String.length digits with
    | 10 -> Some digits
    | 11 -> if digits.[0] = '1' then Some (String.suffix digits 10) else None
    | _ -> None

let area_code_from_cleaned s = String.prefix s 3

let area_code = Option.map ~f:area_code_from_cleaned << number

let pretty =
  let prettify cleaned =
    let area = area_code_from_cleaned cleaned
    in let prefix = String.slice cleaned 3 6
    in let suffix = String.drop_prefix cleaned 6
    in sprintf "(%s) %s-%s" area prefix suffix
  in Option.map ~f:prettify << number
