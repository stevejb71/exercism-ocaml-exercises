open Core.Std

module M = Char.Map

let count dna n = String.count ~f:((=) n) dna

let nucleotide_counts dna =
  let counts = Array.create ~len:(Char.to_int 'T' + 1) 0 in
  for i = 0 to String.length dna - 1 do
    let index = Char.to_int dna.[i]
    in Array.unsafe_set counts index ((Array.unsafe_get counts index) + 1)
  done;
  let add_if_not_zero ncs n =
    let v = Array.get counts (Char.to_int n)
    in match v with
    | 0 -> ncs
    | _ -> Map.add ncs ~key:n ~data:v
  in List.fold ['A';'C';'G';'T'] ~init:M.empty ~f:add_if_not_zero
