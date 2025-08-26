include Stdlib.StringLabels

let sexp_of_t = Sexplib.Std.sexp_of_string
let t_of_sexp = Sexplib.Std.string_of_sexp
let is_empty t = length t = 0
let append = ( ^ )

let split_lines =
  let back_up_at_newline ~t ~pos ~eol =
    pos := !pos - if !pos > 0 && unsafe_get t (!pos - 1) = '\r' then 2 else 1;
    eol := !pos + 1
  in
  fun t ->
    let n = length t in
    if n = 0
    then []
    else (
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if unsafe_get t !pos = '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if not (unsafe_get t !pos = '\n')
        then decr pos
        else (
          (* Because [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol)
      done;
      sub t ~pos:0 ~len:!eol :: !ac)
;;

let lfind ?(pos = 0) t ~f =
  let n = length t in
  let rec loop i =
    if i = n then None else if f (unsafe_get t i) then Some i else loop (i + 1)
  in
  loop pos
;;

let rfind ?pos t ~f =
  let pos =
    match pos with
    | Some pos -> pos
    | None -> length t - 1
  in
  let rec loop i =
    if i < 0 then None else if f (unsafe_get t i) then Some i else loop (i - 1)
  in
  loop pos
;;

let drop_prefix t n = if n > length t then "" else sub t ~pos:n ~len:(length t - n)
let prefix t n = if n > length t then t else sub t ~pos:0 ~len:n

let lstrip ?(drop = Char.Ascii.is_whitespace) t =
  match lfind t ~f:(fun c -> not (drop c)) with
  | None -> ""
  | Some 0 -> t
  | Some n -> drop_prefix t n
;;

let rstrip ?(drop = Char.Ascii.is_whitespace) t =
  match rfind t ~f:(fun c -> not (drop c)) with
  | None -> ""
  | Some i when i = length t - 1 -> t
  | Some i -> prefix t (i + 1)
;;
