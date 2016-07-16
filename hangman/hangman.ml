open Core.Std

type t = int

type progress =
    | Win
    | Lose
    | Busy of int

let create word = 0

let feed letter game = ()

let masked_word game = failwith "undefined"

let progress game = failwith "undefined"
