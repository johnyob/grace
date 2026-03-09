include Stdlib.Int64

let is_representable_as_int =
  let min = Int.min_int |> of_int in
  let max = Int.max_int |> of_int in
  fun t -> compare min t <= 0 && compare t max <= 0
;;

let to_int t = if is_representable_as_int t then Some (to_int t) else None
