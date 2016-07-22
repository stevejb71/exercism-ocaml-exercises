open Core.Std
open React

type progress = Win | Lose | Busy of int

type game = {
  word: string;
  guessed: Char.Set.t signal;
  set_guessed: ?step:Step.t -> Char.Set.t -> unit;
  progress: progress signal;
  set_progress: ?step:Step.t -> progress -> unit
}
type t = game

let create word =
  let guessed, set_guessed = S.create ~eq:Set.equal (Char.Set.empty) in
  let progress, set_progress = S.create (Busy 9) in
  {word; guessed; set_guessed; progress; set_progress}

let feed ch game =
  let guessed = S.value game.guessed in
  let progress = S.value game.progress in
  let guessed' = if String.mem game.word ch then Set.add guessed ch else guessed in
  let update_progress = function | Busy 0 -> Lose | Busy n -> Busy (n - 1) | x -> x in
  let progress' =
    if String.for_all ~f:(Set.mem guessed') game.word then Win
    else if Set.length guessed = Set.length guessed' then update_progress progress
    else progress in

  let step = Step.create () in
  game.set_guessed ~step guessed';
  game.set_progress ~step progress';
  Step.execute step

let masked_word game =
  let mask guessed ch = if Set.mem guessed ch then ch else '_' in
  S.map (fun guessed -> String.map ~f:(mask guessed) game.word) game.guessed

let progress game = game.progress
