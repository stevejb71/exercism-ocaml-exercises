open Core.Std

type school = string list Int.Map.t
module M = Int.Map

let create () = M.empty

let add student grade school = M.add_multi school grade student

let grade grade school = M.find school grade |> Option.value ~default:[]

let sort = M.map ~f:(List.sort ~cmp:String.compare)

let to_map = Fn.id