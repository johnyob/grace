open Core

type t = Format.formatter -> unit

let render ppf t = t ppf
let empty _ppf = ()

let repeat n t ppf =
  for _ = 1 to n do
    t ppf
  done
;;

let newline ppf = Fmt.pf ppf "\n"
let sp ppf = Fmt.pf ppf " "
let cut ppf = Fmt.cut ppf ()
let comma ppf = Fmt.comma ppf ()
let semi ppf = Fmt.semi ppf ()

let append t1 t2 ppf =
  t1 ppf;
  t2 ppf
;;

let ( ++ ) = append

let concat ?(sep = cut) ts ppf =
  let is_first = ref true in
  List.iter ts ~f:(fun t ->
      if !is_first then is_first := false else sep ppf;
      t ppf)
;;

let box ?(indent = 0) t ppf =
  Format.(
    pp_open_box ppf indent;
    t ppf;
    pp_close_box ppf ())
;;

let hbox t ppf =
  Format.(
    pp_open_hbox ppf ();
    t ppf;
    pp_close_box ppf ())
;;

let vbox ?(indent = 0) t ppf =
  Format.(
    pp_open_vbox ppf indent;
    t ppf;
    pp_close_box ppf ())
;;

let hvbox ?(indent = 0) t ppf =
  Format.(
    pp_open_hvbox ppf indent;
    t ppf;
    pp_close_box ppf ())
;;

let hovbox ?(indent = 0) t ppf =
  Format.(
    pp_open_hovbox ppf indent;
    t ppf;
    pp_close_box ppf ())
;;

let char chr ppf = Fmt.char ppf chr
let string str ppf = Fmt.string ppf str
let int n ppf = Fmt.int ppf n
let bool b ppf = Fmt.bool ppf b
let styled style t ppf = Fmt.styled style (fun ppf () -> t ppf) ppf ()
let of_pp pp val_ ppf = pp ppf val_

let option ?(none = empty) some opt ppf =
  Option.value ~default:none (Option.map opt ~f:some) ppf
;;

let set_style_renderer r t ppf =
  let curr_r = Fmt.style_renderer ppf in
  Fun.protect
    (fun () ->
      Fmt.set_style_renderer ppf r;
      t ppf)
    ~finally:(fun () -> Fmt.set_style_renderer ppf curr_r)
;;
