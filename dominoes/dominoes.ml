(* Based off the Haskell solution by Tarmean at http://exercism.io/submissions/6dc2eef7e7eb469d8657111fc4389fc0 *)

open Core.Std

(* Functions from Haskell that I can't find in Core *)

let tails (xs: 'a list): ('a list) list =
  let rec go acc = function
  | [] -> [] :: acc
  | (_::xs) as l -> go (l::acc) xs
  in
  List.rev @@ go [] xs

let inits (xs: 'a list): ('a list) list =
  List.rev xs |> tails |> List.map ~f:List.rev |> List.rev

(* The implementation *)

type dominoe = int * int

type 'a seq = 'a Sequence.t

let zip_with (xs: 'a seq) (ys: 'b seq) ~(f: 'a -> 'b -> 'c) = 
  Sequence.zip xs ys |> Sequence.map ~f:(Tuple2.uncurry f)

let left (ds: dominoe list): int = ds |> List.hd_exn |> fst
let right (ds: dominoe list): int = ds |> List.last_exn |> snd
let choose_from (ls: 'a list): ('a * 'a list) seq =
  Sequence.zip (Sequence.of_list ls) @@ 
    zip_with ~f:List.append (inits ls |> Sequence.of_list) 
                            (List.tl_exn (tails ls) |> Sequence.of_list)

let rec attach_to (path: dominoe list) ((a, b), rest): dominoe list seq =
  let lp = left path in
  if b = lp then go rest ((a,b)::path)
  else if a = lp then go rest ((b,a)::path)
  else Sequence.empty
and go (stones: dominoe list) (path: dominoe list): dominoe list seq = match stones with
| [] -> if left path = right path then Sequence.singleton path else Sequence.empty
| _ -> let open Sequence.Monad_infix in choose_from stones >>= attach_to path

let chain_non_empty (first: dominoe) (rest: dominoe list): (dominoe list) option = 
  Sequence.hd @@ go rest [first]

let chain = function
| [] -> Some []
| first::rest -> chain_non_empty first rest