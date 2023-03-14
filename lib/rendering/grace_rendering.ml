(* open Core *)
module Config = Config
module Snippet = Snippet

let ppd_rich ~config ~files diagnostic =
  let snippet = View.rich ~files diagnostic in
  (* Fmt.pr "Snippet: %a" Sexp.pp_hum (Snippet.sexp_of_t snippet); *)
  Renderer.(render (module Default) { config } snippet)
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
        _ _ => num|}
;;

let fizz_diagnostic : Files.t * Diagnostic.t =
  let files = Files.create () in
  let id = Files.add files "FizzBuzz.fun" source in
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
      ; notes = [ (fun ppf -> Fmt.pf ppf "expected type `String`, found type `Nat`") ]
      } )
;;

let ppd_fizzbuzz =
  let files, diagnostic = fizz_diagnostic in
  ppd_rich ~config:Config.default ~files diagnostic
;;
