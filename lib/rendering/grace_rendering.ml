open Core
module Config = Config
module Snippet = Snippet

let ppd_rich ~config ~files diagnostic =
  let snippet = Diagnostic_to_snippet.rich ~files diagnostic in
  Fmt.pr "@[Snippet: %a@]\n" Sexp.pp_hum (Snippet.sexp_of_t snippet);
  Snippet.ppd ~config snippet
;;

let ppd_simple ~config ~files diagnostic =
  let snippet = Diagnostic_to_snippet.simple ~files diagnostic in
  Snippet.ppd ~config snippet
;;

open Grace

let source =
  {|module FizzBuzz where

fizz1 : Nat -> String
fizz1 num = case (mod num 5) (mod num 3) of
    0 0 => "FizzBuzz"
    0 _ => "Fizz"
    _ 0 => "Buzz"
    _ _ => num

fizz2 : Nat -> String
fizz2 num =
    case (mod num 5) (mod num 3) of
        0 0 => "FizzBuzz"
        0 _ => "Fizz"
        _ 0 => "Buzz"
        _ _ => num
|}
;;

let fizz_diagnostic : File.Cache.t * Diagnostic.t =
  let files = File.Cache.create () in
  let id = File.Cache.add files "FizzBuzz.fun" source in
  let range start stop =
    Range.create (Byte_index.create start) (Byte_index.create stop)
  in
  ( files
  , Diagnostic.
      { severity = Error
      ; message = (fun ppf -> Fmt.pf ppf "`case` clauses have incompatible types")
      ; labels =
          [ Label.primary ~id ~range:(range 318 321) (fun ppf ->
                Fmt.pf ppf "expected `String`, found `Nat`")
          ; Label.secondary ~id ~range:(range 201 321) (fun ppf ->
                Fmt.pf ppf "`case` clauses have incompatible types")
          ; Label.secondary ~id ~range:(range 248 258) (fun ppf ->
                Fmt.pf ppf "this is found to be of type `String`")
          ; Label.secondary ~id ~range:(range 274 280) (fun ppf ->
                Fmt.pf ppf "this is found to be of type `String`")
          ; Label.secondary ~id ~range:(range 296 302) (fun ppf ->
                Fmt.pf ppf "this is found to be of type `String`")
          ; Label.secondary ~id ~range:(range 178 184) (fun ppf ->
                Fmt.pf ppf "expected type `String` found here")
          ]
      ; notes = [ (fun ppf -> Fmt.pf ppf "expected type `String`\nfound type `Nat`") ]
      } )
;;

let ppd_fizzbuzz =
  let files, diagnostic = fizz_diagnostic in
  ppd_rich ~config:Config.default ~files diagnostic
;;

(* 
let source =
  {|

fizz2 : Nat -> String
fizz2 num = 
    case (mod num 5) (mod num 3) of
        0 0 => "FizzBuzz"
        0 _ => "Fizz"
        _ 0 => "Buzz"
        _ _ => num
|}
;;

open Grace
open Diagnostic
open Text

let fizz_snippet : Snippet.t =
  let files = File.Cache.create () in
  let file_id = File.Cache.add files "FizzBuzz.fun" source in
  let dummy_range = Range.create Byte_index.initial Byte_index.initial in
  let label1 : Label.t =
    { id = file_id
    ; range = dummy_range
    ; priority = Secondary
    ; message = (fun ppf -> Fmt.pf ppf "expected type `String` found here")
    }
  in
  let label2 : Label.t =
    { id = file_id
    ; range = dummy_range
    ; priority = Secondary
    ; message = (fun ppf -> Fmt.pf ppf "this is found to be of type `String`")
    }
  in
  let label3 : Label.t = label2
  and label4 = label2 in
  let label5 : Label.t =
    { id = file_id
    ; range = dummy_range
    ; priority = Primary
    ; message = (fun ppf -> Fmt.pf ppf "expected `String`, found `Nat`")
    }
  in
  let label6 : Label.t =
    { id = file_id
    ; range = dummy_range
    ; priority = Secondary
    ; message = (fun ppf -> Fmt.pf ppf "`case` clauses have incompatible types")
    }
  in
  { severity = Error
  ; message = (fun ppf -> Fmt.pf ppf "`case` clauses have incompatible types")
  ; files =
      [ { locus = { file_name = "FizzBuzz.fun"; loc = { line = 16; column = 16 } }
        ; lines =
            [ Source_line
                { line = 10
                ; gutters = { vert_gutters = [ None ]; rightmost_gutter = None }
                ; source = [ "fizz2 : Nat -> ", None; "String", Some Secondary ]
                }
            ; Single_label
                { gutters = [ None ]
                ; column_span = Span.Column_number.create 16 22
                ; carets = List.init 6 (fun _ -> Some Priority.Secondary)
                ; trailing_label =
                    Some (Secondary, Span.Column_number.create 16 22, label1)
                ; hanging_labels = []
                }
            ; Source_line
                { line =
                    11
                    (* problem: no way to differentiate between zero gutters and 1 gutter *)
                ; gutters = { vert_gutters = [ None ]; rightmost_gutter = None }
                ; source = [ "fizz2 num =", None ]
                }
            ; Source_line
                { line = 12
                ; gutters =
                    { vert_gutters = []; rightmost_gutter = Some (`Top, Secondary) }
                ; source = [ "    case (mod num 5) (mod num 3) of", None ]
                }
            ; Source_line
                { line = 13
                ; gutters = { vert_gutters = [ Some Secondary ]; rightmost_gutter = None }
                ; source = [ "        0 0 => ", None; "\"FizzBuzz\"", Some Secondary ]
                }
            ; Single_label
                { gutters = [ Some Secondary ]
                ; column_span = Span.Column_number.create 16 26
                ; carets = List.init 10 (fun _ -> Some Priority.Secondary)
                ; trailing_label =
                    Some (Secondary, Span.Column_number.create 16 26, label2)
                ; hanging_labels = []
                }
            ; Source_line
                { line = 14
                ; gutters = { vert_gutters = [ Some Secondary ]; rightmost_gutter = None }
                ; source = [ "        0 _ => ", None; "\"Fizz\"", Some Secondary ]
                }
            ; Single_label
                { gutters = [ Some Secondary ]
                ; column_span = Span.Column_number.create 16 22
                ; carets = List.init 6 (fun _ -> Some Priority.Secondary)
                ; trailing_label =
                    Some (Secondary, Span.Column_number.create 16 22, label3)
                ; hanging_labels = []
                }
            ; Source_line
                { line = 15
                ; gutters = { vert_gutters = [ Some Secondary ]; rightmost_gutter = None }
                ; source = [ "        _ 0 => ", None; "\"Buzz\"", Some Secondary ]
                }
            ; Single_label
                { gutters = [ Some Secondary ]
                ; column_span = Span.Column_number.create 16 22
                ; carets = List.init 6 (fun _ -> Some Priority.Secondary)
                ; trailing_label =
                    Some (Secondary, Span.Column_number.create 16 22, label4)
                ; hanging_labels = []
                }
            ; Source_line
                { line = 16
                ; gutters = { vert_gutters = [ Some Secondary ]; rightmost_gutter = None }
                ; source = [ "        _ _ => ", None; "num", Some Primary ]
                }
            ; Single_label
                { gutters = [ Some Secondary ]
                ; column_span = Span.Column_number.create 16 19
                ; carets = List.init 3 (fun _ -> Some Priority.Primary)
                ; trailing_label =
                    Some (Secondary, Span.Column_number.create 16 19, label5)
                ; hanging_labels = []
                }
            ; Multi_label
                { gutters = [ Some (`Bottom, Secondary) ]
                ; gutter_idx = 0
                ; priority = Secondary
                ; kind = Bottom (18, label6)
                }
            ]
        }
      ]
  ; notes = [ (fun ppf -> Fmt.pf ppf "expected type `String`\nfound type `Nat`") ]
  }
;;

let ppd_fizzbuzz = Snippet.ppd ~config:Config.default fizz_snippet *)
