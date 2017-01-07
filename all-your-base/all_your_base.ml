open Core.Std

module MonadOps (M : Monad.Basic) = struct
  open M
  let (>>=) = M.bind
  let foldM (xs: 'a list) ~(init: 'b) ~f =
    List.fold xs ~init:(return init) ~f:(fun b a -> (b >>= (fun b -> f b a)))
end

module OptionMonad = struct
  type 'a t = 'a option
  let bind = Option.bind
  let return = Option.return
  let map = `Custom Option.map
end

module OptionMonadOps = MonadOps(OptionMonad)

type base = int

let rec to_digits (b: base) (acc: int list) (n: int): int list =
  let ds = match n with
  | 0 -> acc
  | n -> to_digits b (n % b :: acc) (n / b) in
  if List.is_empty ds then [0] else ds

let convert_bases ~from ~digits ~target =
  if from <= 1 || target <= 1 || List.is_empty digits
  then None
  else
    OptionMonadOps.foldM digits ~init:0 ~f:(
      fun acc d -> if d < 0 || d >= from then None else Some (acc * from + d)
    )
    |> Option.map ~f:(to_digits target [])
