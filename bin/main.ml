open Grace

let source =
  {|
let fizz n = 
  match n mod 5, n mod 3 with
  | 0, 0 -> `Fizz_buzz
  | 0, _ -> `Fizz
  | _, 0 -> `Buzz
  | _, _ -> n
;;
|}
;;

(* Grace provides a Files API for in-memory representations of 
   files. *)
let files = Files.create ()
let fizz = Files.add files "fizz.ml" source

let diagnostic : Diagnostic.t =
  let range start stop =
    Range.create (Byte_index.create start) (Byte_index.create stop)
  in
  Diagnostic.
    { severity = Error
    ; message = (fun ppf -> Fmt.pf ppf "`match` cases have incompatible types")
    ; labels =
        [ Label.primary ~id:fizz ~range:(range 116 117) (fun ppf ->
              Fmt.pf ppf "expected `[> `Buzz | `Fizz | `Fizz_buzz]`, found `int`")
        ; Label.secondary ~id:fizz ~range:(range 17 117) (fun ppf ->
              Fmt.pf ppf "`match` cases have incompatible types")
        ; Label.secondary ~id:fizz ~range:(range 57 67) (fun ppf ->
              Fmt.pf ppf "this is found to be of type `[> `Fizz_buzz]`")
        ; Label.secondary ~id:fizz ~range:(range 80 85) (fun ppf ->
              Fmt.pf ppf "this is found to be of type `[> `Fizz]`")
        ; Label.secondary ~id:fizz ~range:(range 98 103) (fun ppf ->
              Fmt.pf ppf "this is found to be of type `[> `Buzz]`")
        ]
    ; notes = []
    }
;;

let () =
  Fmt_doc.render
    Fmt.stdout
    Grace_rendering.(ppd_rich ~config:Config.default ~files diagnostic)
;;
