open Core.Std
open Core_bench.Std
open Dna

let generate_string (s: string) (n: int): string =
  let len = String.length s in
  let a = Array.init n ~f:(fun i -> s.[i % len]) in
  let l = Array.to_list a in
  String.of_char_list l

let generate_dna = generate_string "ACGT"

let n100 = generate_dna 100
let n1000 = generate_dna 1000
let n1000000 = generate_dna 1000000

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed
      ~name:"nucleotide_counts"
      ~args:[10; 1_000; 10_000; 1_000_000]
      (fun size ->
        let str = generate_dna size in
        Staged.stage (fun () -> Dna.nucleotide_counts str)
        )
  ])


(* Array based
stevejb71, using safe Array lookup
   ┌───────────────────────────────────────────────┬────────────────┬─────────┬────────────┐
   │ Name                                          │       Time/Run │ mWd/Run │ Percentage │
   ├───────────────────────────────────────────────┼────────────────┼─────────┼────────────┤
   │ nucleotide_counts on string of length 100     │       263.55ns │ 179.00w │      0.02% │
   │ nucleotide_counts on string of length 1000    │     1_434.64ns │ 179.00w │      0.11% │
   │ nucleotide_counts on string of length 1000000 │ 1_307_256.61ns │ 179.01w │    100.00% │
   └───────────────────────────────────────────────┴────────────────┴─────────┴────────────┘
stevejb71, using unsafe Array lookup
┌───────────────────────────────────────────────┬────────────────┬─────────┬────────────┐
│ Name                                          │       Time/Run │ mWd/Run │ Percentage │
├───────────────────────────────────────────────┼────────────────┼─────────┼────────────┤
│ nucleotide_counts on string of length 100     │       238.03ns │ 179.00w │      0.02% │
│ nucleotide_counts on string of length 1000    │     1_181.46ns │ 179.00w │      0.11% │
│ nucleotide_counts on string of length 1000000 │ 1_050_891.91ns │ 179.01w │    100.00% │
└───────────────────────────────────────────────┴────────────────┴─────────┴────────────┘
*)
