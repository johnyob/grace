(* Added in OCaml 5.2 *)
let[@warning "-32"] pp_infinity = 1000000010

(* Added in OCaml 5.4 *)
let[@warning "-32"] utf_8_scalar_width s ~pos ~len =
  let rec width s count current stop =
    if current >= stop
    then count
    else (
      let decode = Stdlib.String.get_utf_8_uchar s current in
      let advance = Uchar.utf_decode_length decode in
      width s (count + 1) (current + advance) stop)
  in
  width s 0 pos (pos + len)
;;

include Stdlib.Format
