let rec fold ~init ~f xs = match xs with
  | [] -> init
  | x::xs -> fold ~init:(f init x) ~f:f xs

let flipped_cons xs x = x :: xs

let length xs = fold ~init:0 ~f:(fun len _ -> len + 1) xs

let reverse xs = fold ~init:[] ~f:flipped_cons xs

let map ~f xs = fold ~init:[] ~f:(fun xs x -> (f x)::xs) xs |> reverse

let filter ~f xs = fold ~init:[] ~f:(fun xs x -> if f x then x::xs else xs) xs |> reverse

let append xs ys = fold ~init:ys ~f:flipped_cons (reverse xs)

let concat xss = fold ~init:[] ~f:(fun acc x -> append x acc) (reverse xss)
