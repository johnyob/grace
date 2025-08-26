include Stdlib.StringLabels

let split_lines s =
  let[@tail_mod_cons] rec loop ~last_is_cr i j =
    if j = length s
    then if i = j || (j = i + 1 && last_is_cr) then [] else [ sub s ~pos:i ~len:(j - i) ]
    else (
      match unsafe_get s j with
      | '\r' -> loop ~last_is_cr:true i (j + 1)
      | '\n' ->
        let line =
          let len = if last_is_cr then j - i - 1 else j - i in
          sub s ~pos:i ~len
        in
        line :: loop ~last_is_cr:false (i + 1) (j + 1)
      | _ -> loop ~last_is_cr:false i (j + 1))
  in
  loop ~last_is_cr:false 0 0
;;
