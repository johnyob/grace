let length s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec loop acc =
    match Uutf.decode decoder with
    | `Uchar _ -> loop (acc + 1)
    | `End -> acc
    | `Malformed _ -> raise (Invalid_argument "invalid UTF-8")
    | `Await -> assert false
  in
  loop 0
;;
