open Core.Std
open OUnit2
open Dominoes

let print_dominoe (d1, d2) = sprintf "(%d,%d)" d1 d2

let option_printer = function
  | None -> "None"
  | Some xs -> "Some [" ^ String.concat ~sep:";" (List.map xs ~f:print_dominoe) ^ "]"

let drop_1_right xs = List.rev xs |> List.tl_exn |> List.rev

let check_chain (chained: dominoe list) = 
  let assert_dominoes_match d1 d2 =
    if snd d1 <> fst d2 then failwith @@ sprintf "%s and %s cannot be chained together" (print_dominoe d1) (print_dominoe d2) else () in
  let consecutives = List.zip_exn (drop_1_right chained) (List.tl_exn chained) in
  List.iter consecutives ~f:(fun (d1, d2) -> assert_dominoes_match d1 d2)

let assert_empty c = if List.is_empty c then () else failwith "Expected 0 length chain"

let assert_valid_chain input _ctxt =
  match chain input with
  | None -> failwith "Expecting a chain"
  | Some(c) -> (if List.is_empty input then assert_empty else check_chain) c  

let assert_no_chain input _ctxt =
  assert_equal None (chain input) ~printer:option_printer

let assert_chain input hasChain =
  if hasChain then assert_valid_chain input else assert_no_chain input

let tests = [
  "empty input = empty output" >::
    assert_chain [] true;
  "singleton input = singleton output" >::
    assert_chain [(1,1)] true;
  "singleton that can't be chained" >::
    assert_chain [(1,2)] false;
  "three elements" >::
    assert_chain [(1,2); (3,1); (2,3)] true;
  "can reverse dominoes" >::
    assert_chain [(1,2); (1,3); (2,3)] true;
  "can't be chained" >::
    assert_chain [(1,2); (4,1); (2,3)] false;
  "disconnected - simple" >::
    assert_chain [(1,1); (2,2)] false;
  "disconnected - double loop" >::
    assert_chain [(1,2); (2,1); (3,4); (4,3)] false;
  "disconnected - single isolated" >::
    assert_chain [(1,2); (2,3); (3,1); (4,4)] false;
  "need backtrack" >::
    assert_chain [(1,2); (2,3); (3,1); (2,4); (2,4)] true;
  "separate loops" >::
    assert_chain [(1,2); (2,3); (3,1); (1,1); (2,2); (3,3)] true;
  "ten elements" >::
    assert_chain [(1,2); (5,3); (3,1); (1,2); (2,4); (1,6); (2,3); (3,4); (5,6)] true;
]

let () =
  run_test_tt_main ("dominoes tests" >::: tests)