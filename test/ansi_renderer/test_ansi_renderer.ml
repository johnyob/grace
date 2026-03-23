open! Core
open! Grace
open! Grace_std
open Diagnostic

let source name content : Source.t =
  `String { name = Some name; content = Dedent.string content }
;;

let range ~source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)
;;

(* This code is Copyright (c) 2017 The b0 programmers.
  SPDX-License-Identifier: ISC *)
let strip_ansi_escapes s =
  let len = String.length s in
  let b = Buffer.create len in
  let max = len - 1 in
  let flush start stop =
    if start < 0 || start > max
    then ()
    else Stdlib.Buffer.add_substring b s start (stop - start + 1)
  in
  let rec skip_esc i =
    if i > max
    then loop i i
    else (
      let k = i + 1 in
      if s.[i] = 'm' then loop k k else skip_esc k)
  and loop start i =
    match i > max with
    | true ->
      if Buffer.length b = len
      then s
      else (
        flush start max;
        Buffer.contents b)
    | false ->
      (match s.[i] with
       | '\x1B' ->
         flush start (i - 1);
         skip_esc (i + 1)
       | _ -> loop start (i + 1))
  in
  loop 0 0
;;

let pr_diagnostics
      ?(config = Grace_ansi_renderer.Config.default)
      ?(ansi = false)
      diagnostics
  =
  let open Grace_ansi_renderer in
  let config = { config with use_ansi = Some ansi } in
  let output =
    Fmt.(str_like stdout)
      "%a"
      Fmt.(
        list
          ~sep:(fun ppf () -> pf ppf "@.@.@.")
          (fun ppf diagnostic ->
             pp_diagnostic ~config ppf diagnostic;
             pf ppf "@.@.";
             pp_compact_diagnostic ~config ppf diagnostic))
      diagnostics
  in
  print_endline (strip_ansi_escapes output)
;;

let pr_bad_diagnostics ?(config = Grace_ansi_renderer.Config.default) diagnostics =
  let open Grace_ansi_renderer in
  let config = { config with use_ansi = Some false } in
  Fmt.(
    list
      ~sep:(fun ppf () -> pf ppf "@.@.")
      (fun ppf diagnostic ->
         try pp_diagnostic ~config ppf diagnostic with
         | exn -> Fmt.pf ppf "Raised: %s" (Exn.to_string exn)))
    Fmt.stdout
    diagnostics
;;

(* Taken from https://github.com/brendanzab/codespan/blob/master/codespan-reporting/tests/term.rs *)

let%expect_test "empty" =
  let diagnostics =
    let empty severity =
      Diagnostic.
        { severity
        ; message = (fun ppf -> Fmt.pf ppf "")
        ; labels = []
        ; notes = []
        ; code = None
        }
    in
    List.map ~f:empty Severity.[ Help; Note; Warning; Error; Bug ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    help:

    help:


    note:

    note:


    warning:

    warning:


    error:

    error:


    bug:

    bug: |}]
;;

let%expect_test "same_line" =
  let source =
    source
      "one_line.rs"
      {|
      > fn main() {
      >     let mut v = vec![Some("foo"), Some("bar")];
      >     v.push(v.pop().unwrap());
      > }
      |}
  in
  let diagnostics =
    [ Diagnostic.createf
        ~labels:
          [ Label.primaryf
              ~range:(range ~source 71 72)
              "second mutable borrow occurs here"
          ; Label.secondaryf
              ~range:(range ~source 64 65)
              "first borrow later used by call"
          ; Label.secondaryf
              ~range:(range ~source 66 70)
              "first mutable borrow occurs here"
          ]
        Error
        "cannot borrow `v` as mutable more than once at a time"
    ; Diagnostic.createf
        ~notes:
          [ Message.create "For more information about this error, try `rustc --explain`"
          ]
        Error
        "aborting due to previous error"
    ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: cannot borrow `v` as mutable more than once at a time
        ┌─ one_line.rs:3:12
      3 │      v.push(v.pop().unwrap());
        │      - ---- ^ second mutable borrow occurs here
        │      │ │
        │      │ first mutable borrow occurs here
        │      first borrow later used by call

    one_line.rs:3:12: error: cannot borrow `v` as mutable more than once at a time


    error: aborting due to previous error
        = For more information about this error, try `rustc --explain`

    error: aborting due to previous error
     = For more information about this error, try `rustc --explain`
    |}]
;;

let%expect_test "overlapping" =
  let s1 =
    source
      "nested_impl_trait.rs"
      {|
      > use std::fmt::Debug;
      >
      > fn fine(x: impl Into<u32>) -> impl Into<u32> { x }
      >
      > fn bad_in_ret_position(x: impl Into<u32>) -> impl Into<impl Debug> { x }
      |}
  in
  let s2 =
    source
      "typeck_type_placeholder_item.rs"
      {|
      > fn fn_test1() -> _ { 5 }
      > fn fn_test2(x: i32) -> (_, _) { (x, x) }
      |}
  in
  let s3 =
    source
      "libstd/thread/mod.rs"
      {|
      > #[stable(feature = "rust1", since = "1.0.0")]
      > pub fn spawn<F, T>(self, f: F) -> io::Result<JoinHandle<T>>
      > where
      >     F: FnOnce() -> T,
      >     F: Send + 'static,
      >     T: Send + 'static,
      > {
      >  unsafe { self.spawn_unchecked(f) }
      > }
      |}
  in
  let s4 =
    source
      "no_send_res_ports.rs"
      {|
      > use std::thread;
      > use std::rc::Rc;
      >
      > #[derive(Debug)]
      > struct Port<T>(Rc<T>);
      >
      > fn main() {
      >     #[derive(Debug)]
      >     struct Foo {
      >         _x: Port<()>,
      >     }
      >
      >     impl Drop for Foo {
      >         fn drop(&mut self) {}
      >     }
      >
      >     fn foo(x: Port<()>) -> Foo {
      >         Foo {
      >             _x: x
      >         }
      >     }
      >
      >     let x = foo(Port(Rc::new(())));
      >
      >     thread::spawn(move|| {
      >         let y = x;
      >         println!("{:?}", y);
      >     });
      > }
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source:s1 129 139) "nested `impl Trait` here"
              ; secondaryf ~range:(range ~source:s1 119 140) "outer `impl Trait`"
              ]
          Error
          "nested `impl Trait` is not allowed"
      ; createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source:s2 17 18) "not allowed in type signatures"
              ; secondaryf
                  ~range:(range ~source:s2 17 18)
                  "help: replace with the correct return type: `i32`"
              ]
          Error
          "the type placeholder `_` is not allowed within types on item signatures"
      ; createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source:s2 49 50) "not allowed in type signatures"
              ; primaryf ~range:(range ~source:s2 52 53) "not allowed in type signatures"
              ; secondaryf
                  ~range:(range ~source:s2 48 54)
                  "help: replace with the correct return type: `(i32, i32)`"
              ]
          Error
          "the type placeholder `_` is not allowed within types on item signatures"
      ; createf
          ~labels:
            Label.
              [ primaryf
                  ~range:(range ~source:s4 339 352)
                  "`std::rc::Rc<()> cannot be sent between threads safely`"
              ; secondaryf
                  ~range:(range ~source:s4 353 416)
                  "within this `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
              ; secondaryf
                  ~range:(range ~source:s3 141 145)
                  "required by this bound in `std::thread::spawn`"
              ]
          ~notes:
            Message.
              [ create
                  "help: within `[closure@no_send_res_ports.rs:29:19: 33:6 \
                   x:main::Foo]`, the trait `std::marker::Send` is not implemented for \
                   `std::rc::Rc<()>`"
              ; create "note: required because it appears within the type `Port<()>`"
              ; create "note: required because it appears within the type `main::Foo`"
              ; create
                  "note: required because it appears within the type \
                   `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
              ]
          Error
          "`std::rc::Rc<()>` cannot be sent between threads safely"
      ; createf
          ~notes:
            Message.
              [ create "Some errors have detailed explanations: ..."
              ; create "For more information about an error, try `rustc --explain`"
              ]
          Error
          "aborting due 5 previous errors"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: nested `impl Trait` is not allowed
        ┌─ nested_impl_trait.rs:5:56
      5 │  fn bad_in_ret_position(x: impl Into<u32>) -> impl Into<impl Debug> { x }
        │                                               ----------^^^^^^^^^^-
        │                                               │         │
        │                                               │         nested `impl Trait` here
        │                                               outer `impl Trait`

    nested_impl_trait.rs:5:56: error: nested `impl Trait` is not allowed


    error: the type placeholder `_` is not allowed within types on item signatures
        ┌─ typeck_type_placeholder_item.rs:1:18
      1 │  fn fn_test1() -> _ { 5 }
        │                   ^
        │                   │
        │                   not allowed in type signatures
        │                   help: replace with the correct return type: `i32`

    typeck_type_placeholder_item.rs:1:18: error: the type placeholder `_` is not allowed within types on item signatures


    error: the type placeholder `_` is not allowed within types on item signatures
        ┌─ typeck_type_placeholder_item.rs:2:28
      2 │  fn fn_test2(x: i32) -> (_, _) { (x, x) }
        │                         -^--^-
        │                         ││  │
        │                         ││  not allowed in type signatures
        │                         │not allowed in type signatures
        │                         help: replace with the correct return type: `(i32, i32)`

    typeck_type_placeholder_item.rs:2:28: error: the type placeholder `_` is not allowed within types on item signatures


    error: `std::rc::Rc<()>` cannot be sent between threads safely
        ┌─ libstd/thread/mod.rs:5:8
      5 │        F: Send + 'static,
        │           ---- required by this bound in `std::thread::spawn`
        ┌─ no_send_res_ports.rs:25:5
     24 │
     25 │        thread::spawn(move|| {
        │        ^^^^^^^^^^^^^ `std::rc::Rc<()> cannot be sent between threads safely`
        │ ╭────────────────────'
     26 │ │          let y = x;
     27 │ │          println!("{:?}", y);
     28 │ │      });
        │ ╰───────' within this `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`
     29 │    }
        = help: within `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`, the trait `std::marker::Send` is not implemented for `std::rc::Rc<()>`
        = note: required because it appears within the type `Port<()>`
        = note: required because it appears within the type `main::Foo`
        = note: required because it appears within the type `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`

    libstd/thread/mod.rs:5:8: error: `std::rc::Rc<()>` cannot be sent between threads safely
    no_send_res_ports.rs:25:5: error: `std::rc::Rc<()>` cannot be sent between threads safely
     = help: within `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`, the trait `std::marker::Send` is not implemented for `std::rc::Rc<()>`
     = note: required because it appears within the type `Port<()>`
     = note: required because it appears within the type `main::Foo`
     = note: required because it appears within the type `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`


    error: aborting due 5 previous errors
        = Some errors have detailed explanations: ...
        = For more information about an error, try `rustc --explain`

    error: aborting due 5 previous errors
     = Some errors have detailed explanations: ...
     = For more information about an error, try `rustc --explain`
    |}]
;;

let%expect_test "same ranges" =
  let source = source "same_range" "::S { }" in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            Label.
              [ primaryf ~range:(range ~source 4 5) "Unexpected '{'"
              ; secondaryf ~range:(range ~source 4 5) "Expected '('"
              ]
          Error
          "unexpected token"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: unexpected token
        ┌─ same_range:1:5
      1 │  ::S { }
        │      ^
        │      │
        │      Unexpected '{'
        │      Expected '('

    same_range:1:5: error: unexpected token
    |}]
;;

let%expect_test "multiline_overlapping" =
  let source =
    source
      "file.rs"
      {|
      >         match line_index.compare(self.last_line_index()) {
      >             Ordering::Less => Ok(self.line_starts()[line_index.to_usize()]),
      >             Ordering::Equal => Ok(self.source_span().end()),
      >             Ordering::Greater => LineIndexOutOfBoundsError {
      >                 given: line_index,
      >                 max: self.last_line_index()
      >             },
      >         }
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~notes:
            [ Message.create
                "expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found \
                 `LineIndexOutOfBoundsError`"
            ]
          ~labels:
            Label.
              [ secondaryf
                  ~range:(range ~source 89 134)
                  "this is found to be of type `Result<ByteIndex, \
                   LineIndexOutOfBoundsError>`"
              ; primaryf
                  ~range:(range ~source 230 350)
                  "expected enum `Result`, found struct `LineIndexOutOfBoundsError`"
              ; secondaryf
                  ~range:(range ~source 8 361)
                  "`match` arms have incompatible types"
              ; secondaryf
                  ~range:(range ~source 167 195)
                  "this is found to be of type `Result<ByteIndex, \
                   LineIndexOutOfBoundsError>`"
              ]
          Error
          "match arms have incompatible types"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: match arms have incompatible types
        ┌─ file.rs:4:34
      1 │ ╭            match line_index.compare(self.last_line_index()) {
      2 │ │                Ordering::Less => Ok(self.line_starts()[line_index.to_usize()]),
        │ │                                  --------------------------------------------- this is found to be of type `Result<ByteIndex, LineIndexOutOfBoundsError>`
      3 │ │                Ordering::Equal => Ok(self.source_span().end()),
        │ │                                   ---------------------------- this is found to be of type `Result<ByteIndex, LineIndexOutOfBoundsError>`
      4 │ │                Ordering::Greater => LineIndexOutOfBoundsError {
        │ │ ╭───────────────────────────────────^
      5 │ │ │                  given: line_index,
      6 │ │ │                  max: self.last_line_index()
      7 │ │ │              },
        │ │ ╰──────────────^ expected enum `Result`, found struct `LineIndexOutOfBoundsError`
      8 │ │            }
        │ ╰────────────' `match` arms have incompatible types
        = expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found `LineIndexOutOfBoundsError`

    file.rs:4:34: error: match arms have incompatible types
     = expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found `LineIndexOutOfBoundsError`
    |}]
;;

let%expect_test "unicode" =
  let source = source "unicode.rs" {|extern "路濫狼á́́" fn foo() {}|} in
  let diagnostics =
    Diagnostic.
      [ createf
          ~notes:
            [ Message.createf
                "@[<v 3>valid ABIs:@.- aapcs@.- amdgpu-kernel@.- C@.- cdecl@.- efiapi@.- \
                 fastcall@.- msp430-interrupt@.- platform-intrinsic@.- ptx-kernel@.- \
                 Rust@.- rust-call@.- rust-intrinsic@.- stdcall@.- system@.- sysv64@.- \
                 thiscall@.- unadjusted@.- vectorcall@.- win64@.- x86-interrupt@]"
            ]
          ~labels:[ Label.primaryf ~range:(range ~source 7 24) "invalid ABI" ]
          Error
          "invalid ABI: found `路濫狼á́́`"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: invalid ABI: found `路濫狼á́́`
        ┌─ unicode.rs:1:8
      1 │  extern "路濫狼á́́" fn foo() {}
        │         ^^^^^^^^ invalid ABI
        = valid ABIs:
          - aapcs
          - amdgpu-kernel
          - C
          - cdecl
          - efiapi
          - fastcall
          - msp430-interrupt
          - platform-intrinsic
          - ptx-kernel
          - Rust
          - rust-call
          - rust-intrinsic
          - stdcall
          - system
          - sysv64
          - thiscall
          - unadjusted
          - vectorcall
          - win64
          - x86-interrupt

    unicode.rs:1:8: error: invalid ABI: found `路濫狼á́́`
     = valid ABIs:
       - aapcs
       - amdgpu-kernel
       - C
       - cdecl
       - efiapi
       - fastcall
       - msp430-interrupt
       - platform-intrinsic
       - ptx-kernel
       - Rust
       - rust-call
       - rust-intrinsic
       - stdcall
       - system
       - sysv64
       - thiscall
       - unadjusted
       - vectorcall
       - win64
       - x86-interrupt |}]
;;

let%expect_test "unicode spans" =
  let source = source "moon_jump.rs" "🐄🌑🐄🌒🐄🌓🐄🌔🐄🌕🐄🌖🐄🌗🐄🌘🐄" in
  let invalid_start = 1 in
  let invalid_stop = String.length "🐄" - 1 in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf
                ~range:(range ~source invalid_start invalid_stop)
                "Invalid jump"
            ]
          Error
          "Cow may not jump during new moon."
      ; createf
          ~labels:
            [ Label.secondaryf
                ~range:(range ~source invalid_start (String.length "🐄"))
                "Cow range does not start at boundary."
            ]
          Note
          "Invalid unicode range"
      ; createf
          ~labels:
            [ Label.secondaryf
                ~range:(range ~source (String.length "🐄🌑") (String.length "🐄🌑🐄" - 1))
                "Cow range does not end at boundary"
            ]
          Note
          "Invalid unicode range"
      ; createf
          ~labels:
            [ Label.secondaryf
                ~range:(range ~source invalid_start (String.length "🐄🌑🐄" - 1))
                "Cow does not start or end at boundary."
            ]
          Note
          "Invalid unicode range"
      ]
  in
  pr_bad_diagnostics diagnostics;
  [%expect
    {|
    Raised: (Invalid_argument "invalid UTF-8")

    Raised: (Invalid_argument "invalid UTF-8")

    Raised: (Invalid_argument "invalid UTF-8")

    Raised: (Invalid_argument "invalid UTF-8") |}]
;;

let%expect_test "empty range" =
  (* Bug #1: https://github.com/johnyob/grace/issues/1 *)
  let source = source "hello.txt" "Hello world!\nBye world!\n   " in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:[ Label.primaryf ~range:(range ~source 6 6) "middle" ]
          Note
          "empty range"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    note: empty range
        ┌─ hello.txt:1:7
      1 │  Hello world!
        │        ^ middle

    hello.txt:1:7: note: empty range
   |}]
;;

let%expect_test "multiline label starting after newline" =
  (* Bug #49: https://github.com/johnyob/grace/issues/49 *)
  let content = "foo\n\nbar {\n}\n" in
  let source : Source.t = `String { name = None; content } in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf ~range:(range ~source 0 3) "e1"
            ; Label.secondaryf ~range:(range ~source 5 13) "e2"
            ]
          Error
          "err"
      ]
  in
  pr_bad_diagnostics diagnostics;
  [%expect
    {|
    error: err
        ┌─ unknown:1:1
      1 │    foo
        │    ^^^ e1
      2 │
      3 │ ╭  bar {
      4 │ │  }
        │ ╰───' e2
      5 │
    |}]
;;

let%expect_test "label starting on EOF/EOL" =
  (* Bug #37: https://github.com/johnyob/grace/issues/37 *)
  let content = "let x :" in
  let source = source "eof.ml" content in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:[ Label.primaryf ~range:(range ~source 7 7) "Unexpected EOF here" ]
          Error
          "syntax error"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: syntax error
        ┌─ eof.ml:1:8
      1 │  let x :
        │         ^ Unexpected EOF here

    eof.ml:1:8: error: syntax error
    |}]
;;

let%expect_test "multi-line empty messages" =
  let source =
    source
      "rigid_variable_escape.ml"
      {|
      > let escape = fun f ->
      >   fun (type a) ->
      >     (f : a -> a)
      > ;;
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:[ Label.primaryf ~range:(range ~source 24 56) "" ]
          Error
          "generic type variable `a` escapes its scope"
      ]
  in
  pr_diagnostics diagnostics;
  [%expect
    {|
    error: generic type variable `a` escapes its scope
        ┌─ rigid_variable_escape.ml:2:3
      1 │    let escape = fun f ->
      2 │ ╭    fun (type a) ->
      3 │ │      (f : a -> a)
        │ ╰─────────────────^
      4 │    ;;

    rigid_variable_escape.ml:2:3: error: generic type variable `a` escapes its scope
    |}]
;;

let snippet_ml_source : Source.t = `File "inputs/snippet.ml"

let%expect_test "multi-label on large file" =
  let source = snippet_ml_source in
  let diagnostics (a1, b1) (a2, b2) =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf ~range:(range ~source a1 b1) "Error is happening here"
            ; Label.primaryf ~range:(range ~source a2 b2) "Bananad from here"
            ]
          Error
          "Some dramatic error"
      ]
  in
  pr_diagnostics (diagnostics (2951, 3015) (17511, 17537));
  [%expect
    {|
    error: Some dramatic error
        ┌─ inputs/snippet.ml:542:21
    148 │           ~compare:(Comparable.pair Diagnostic.Priority.compare Byte_index.compare)
        │                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Error is happening here
        ·
    542 │          let locus = locus_of_labels ~sd labels in
        │                      ^^^^^^^^^^^^^^^^^^^^^^^^^^ Bananad from here

    inputs/snippet.ml:542:21: error: Some dramatic error
    |}];
  (* Also works with 1- and 2-digits line numbers *)
  pr_diagnostics (diagnostics (1474, 1493) (84, 107));
  [%expect
    {|
    error: Some dramatic error
        ┌─ inputs/snippet.ml:73:18
      5 │  let margin_length_of_string line_content =
        │      ^^^^^^^^^^^^^^^^^^^^^^^ Bananad from here
        ·
     73 │      let length = Utf8.length content in
        │                   ^^^^^^^^^^^^^^^^^^^ Error is happening here

    inputs/snippet.ml:73:18: error: Some dramatic error
    |}];
  (* Check that there is no ellipsis if there is a gap of size 1 *)
  pr_diagnostics (diagnostics (84, 107) (195, 205));
  [%expect
    {|
    error: Some dramatic error
        ┌─ inputs/snippet.ml:7:6
      5 │  let margin_length_of_string line_content =
        │      ^^^^^^^^^^^^^^^^^^^^^^^ Error is happening here
      6 │    (* This is valid for UTF8 as all the whitespace characters we're
      7 │       interested in wrt a 'margin' have a width of 1. *)
        │       ^^^^^^^^^^ Bananad from here

    inputs/snippet.ml:7:6: error: Some dramatic error
    |}]
;;

let num_contextual_lines_test num_contextual_lines =
  let config = Grace_ansi_renderer.Config.{ default with num_contextual_lines } in
  let source =
    source
      "context.ml"
      {|
      > line 1
      > line 2
      > line 3
      > line 4
      > line 5
      > line 6
      > line 7
      > line 8
      > line 9
      > line 10
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf ~range:(range ~source 7 13) "error on line 2"
            ; Label.primaryf ~range:(range ~source 38 44) "error on line 6"
            ]
          Error
          "Testing %d contextual lines"
          num_contextual_lines
      ]
  in
  pr_diagnostics ~config diagnostics
;;

let%expect_test "num_contextual_lines = 0" =
  num_contextual_lines_test 0;
  [%expect
    {|
    error: Testing 0 contextual lines
        ┌─ context.ml:6:4
      2 │    line 2
        │    ^^^^^^ error on line 2
        ·
      6 │    line 6
        │ ╭─────^
      7 │ │  line 7
        │ ╰───^ error on line 6

    context.ml:6:4: error: Testing 0 contextual lines
    |}]
;;

let%expect_test "num_contextual_lines = 2" =
  num_contextual_lines_test 2;
  [%expect
    {|
    error: Testing 2 contextual lines
        ┌─ context.ml:6:4
      2 │    line 2
        │    ^^^^^^ error on line 2
      3 │    line 3
      4 │    line 4
      5 │    line 5
      6 │    line 6
        │ ╭─────^
      7 │ │  line 7
        │ ╰───^ error on line 6
      8 │    line 8
      9 │    line 9

    context.ml:6:4: error: Testing 2 contextual lines
    |}]
;;

let%expect_test "num_contextual_lines = 3" =
  num_contextual_lines_test 3;
  [%expect
    {|
    error: Testing 3 contextual lines
        ┌─ context.ml:6:4
      2 │    line 2
        │    ^^^^^^ error on line 2
      3 │    line 3
      4 │    line 4
      5 │    line 5
      6 │    line 6
        │ ╭─────^
      7 │ │  line 7
        │ ╰───^ error on line 6
      8 │    line 8
      9 │    line 9
     10 │    line 10

    context.ml:6:4: error: Testing 3 contextual lines
    |}]
;;

let enable_inline_contextual_lines_test
      ~enable_inline_contextual_lines
      ~num_contextual_lines
  =
  let source =
    source
      "inline.ml"
      {|
      > line 1
      > line 2
      > line 3
      > line 4
      > line 5
      |}
  in
  let diagnostics =
    Diagnostic.
      [ createf
          ~labels:
            [ Label.primaryf ~range:(range ~source 7 9) "inline"
            ; Label.secondaryf ~range:(range ~source 10 13) "label"
            ]
          Error
          "Testing inline labels without contextual lines"
      ]
  in
  let open Grace_ansi_renderer in
  let config =
    Config.{ default with enable_inline_contextual_lines; num_contextual_lines }
  in
  pr_diagnostics ~config diagnostics
;;

let%expect_test "enable_inline_contextual_lines = false (default)" =
  enable_inline_contextual_lines_test
    ~enable_inline_contextual_lines:false
    ~num_contextual_lines:1;
  [%expect
    {|
    error: Testing inline labels without contextual lines
        ┌─ inline.ml:2:1
      2 │  line 2
        │  ^^ --- label
        │  │
        │  inline

    inline.ml:2:1: error: Testing inline labels without contextual lines
    |}]
;;

let%expect_test "enable_inline_contextual_lines = true" =
  enable_inline_contextual_lines_test
    ~enable_inline_contextual_lines:true
    ~num_contextual_lines:1;
  [%expect
    {|
    error: Testing inline labels without contextual lines
        ┌─ inline.ml:2:1
      1 │  line 1
      2 │  line 2
        │  ^^ --- label
        │  │
        │  inline
      3 │  line 3

    inline.ml:2:1: error: Testing inline labels without contextual lines
    |}]
;;

let%expect_test "num_contextual_lines = 2 and enable_inline_contextual_lines = true" =
  enable_inline_contextual_lines_test
    ~enable_inline_contextual_lines:true
    ~num_contextual_lines:2;
  [%expect
    {|
    error: Testing inline labels without contextual lines
        ┌─ inline.ml:2:1
      1 │  line 1
      2 │  line 2
        │  ^^ --- label
        │  │
        │  inline
      3 │  line 3
      4 │  line 4

    inline.ml:2:1: error: Testing inline labels without contextual lines
|}]
;;

let%expect_test "label with multiple lines and ansi formatting" =
  (* Bug #71: https://github.com/johnyob/grace/issues/71*)
  let compare diagnostic =
    pr_diagnostics ~ansi:true [ diagnostic ];
    (* check consistency with non-unicode, non-ansi *)
    pr_diagnostics
      ~ansi:false
      ~config:Grace_ansi_renderer.Config.{ default with chars = Chars.ascii }
      [ diagnostic ]
  in
  let content = "foo\n\nbar {\n};\nbaz" in
  let source : Source.t = `String { name = None; content } in
  let diagnostic =
    Diagnostic.(
      createf
        ~labels:
          [ Label.primaryf
              ~range:(range ~source 0 3)
              "@[<v2>e1:@ new line of error1@]@ unboxed new line of error 1"
          ; Label.secondaryf
              ~range:(range ~source 5 14)
              "@[<v2>e2:@ new line of error 2@]@ unboxed new line of error 2"
          ]
        Error
        "err")
  in
  compare diagnostic;
  [%expect
    {|
    error: err
        ┌─ unknown:1:1
      1 │    foo
        │    ^^^ e1:
        │          new line of error1
        │    unboxed new line of error 1
      2 │
      3 │ ╭  bar {
      4 │ │  };
        │ ╰────' e2:
                                         new line of error 2
                                       unboxed new line of error 2
      5 │    baz

    unknown:1:1: error: err
    error: err
        --> unknown:1:1
      1 |    foo
        |    ^^^ e1:
        |          new line of error1
        |    unboxed new line of error 1
      2 |
      3 | /  bar {
      4 | |  };
        | \----' e2:
          new line of error 2
        unboxed new line of error 2
      5 |    baz

    unknown:1:1: error: err
    |}]
;;
