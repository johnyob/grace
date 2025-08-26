module Array0 = Stdlib.StdLabels.Array

(* Added in OCaml 5.1 *)
let[@warning "-32"] find_index t ~f =
  let rec loop i =
    if i = Array0.length t
    then None
    else if f (Array0.unsafe_get t i)
    then Some i
    else loop (i + 1)
  in
  loop 0
;;

include Array0
