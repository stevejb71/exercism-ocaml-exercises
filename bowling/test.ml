open Core.Std
open OUnit2
module B = Bowling

let (>>) = Fn.compose

let assert_score e g _test_context =
  assert_equal ~printer:Int.to_string e (B.new_game |> g |> B.score)

let replicate count n =
  let rec go acc count =
    if count = 0 then acc
    else go (n :: acc) (count - 1) in
  go [] count
let roll = B.roll
let roll_many scores game = List.fold ~init:game ~f:(Fn.flip roll) (List.rev scores)
let roll_spare = roll_many [5;5]
let roll_strike = roll 10
let roll_repeatedly (count: int) (score: int) (game: B.t): B.t =
  roll_many (replicate count score) game

let tests = [
  "gutter every frame gives a score of 0" >::
    assert_score 0 (roll_repeatedly 20 0);
  "1 pin every frame gives a score of 20" >::
    assert_score 20 (roll_repeatedly 20 1);
  "with no spares or strikes, the score is the sum of the scores for each frame" >::
    assert_score 54 (roll_many [1;0;0;2;0;3;0;4;0;5;0;6;0;7;0;8;0;9;9;0]);
  "frame score for a spare is 10 plus the number of pins knocked down in the next throw" >::
    assert_score 16 (roll_spare >> roll 3 >> roll_repeatedly 17 0);
  "two spares in a row" >::
    assert_score 35 (roll_spare >> roll_spare >> roll_many [4;2] >> roll_repeatedly 14 0);
  "frame score for a strike is 10 plus the number of pins knocked down in the next 2 throws" >::
    assert_score 24 (roll_strike >> roll_many [3;4] >> roll_repeatedly 16 0);
  "perfect game" >::
    assert_score 300 (roll_repeatedly 12 10);
  "a strike in the 10th frame adds on the fill ball" >::
    assert_score 15 (roll_repeatedly 18 0 >> roll_strike >> roll_many [3;2]);
  "a spare in the 10th frame adds on the fill ball" >::
    assert_score 12 (roll_repeatedly 18 0 >> roll_spare >> roll 2);
  "a spare followed by a strike" >::
    assert_score 48 (roll_spare >> roll_strike >> roll_many [3;6] >> roll_repeatedly 14 0);
  "a strike followed by a spare" >::
    assert_score 42 (roll_strike >> roll_spare >> roll_many [3;6] >> roll_repeatedly 14 0);
  "a strike followed by a spare on the last frame" >::
    assert_score 33 (roll_repeatedly 16 0 >> roll_strike >> roll_spare >> roll 3);
  "a spare followed by a strike on the last frame" >::
    assert_score 36 (roll_repeatedly 16 0 >> roll_spare >> roll_strike >> roll_many [4;2]);
]

let () =
  run_test_tt_main ("bowling tests" >::: tests)
