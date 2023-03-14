open! Grace

(* Grace provides a [Source] API for in-memory representations of sources/files. *)
let fizz : Source.t =
  `String
    { name = Some "fizz.ml"
    ; content =
        {|
let fizz n = 
  match n mod 5, n mod 3 with
  | 0, 0 -> `Fizz_buzz
  | 0, _ -> `Fizz
  | _, 0 -> `Buzz
  | _, _ -> n
;;
|}
    }
;;

let diagnostic =
  let range start stop =
    Range.create ~source:fizz (Byte_index.of_int start) (Byte_index.of_int stop)
  in
  Diagnostic.(
    createf
      ~labels:
        Label.
          [ primaryf
              ~range:(range 116 117)
              "expected `[> `Buzz | `Fizz | `Fizz_buzz]`, found `int`"
          ; secondaryf ~range:(range 17 117) "`match` cases have incompatible types"
          ; secondaryf ~range:(range 57 67) "this is found to be of type `[> `Fizz_buzz]`"
          ; secondaryf ~range:(range 80 85) "this is found to be of type `[> `Fizz]`"
          ; secondaryf ~range:(range 98 103) "this is found to be of type `[> `Buzz]`"
          ]
      Error
      "`match` cases have incompatible types")
;;

let () =
  Format.printf
    "%a@."
    Grace_rendering.(Ansi.pp_diagnostic ~config:Config.default)
    diagnostic
;;
