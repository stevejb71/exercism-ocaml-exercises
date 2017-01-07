open Core.Std

module Make (M : Monad.S) = struct
  open M
  let foldM (xs: 'a list) ~(init: 'b) ~f =
    List.fold xs ~init:(return init) ~f:(fun b a -> (b >>= (fun b -> f b a)))
end
