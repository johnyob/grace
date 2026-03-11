open! Core
open! Grace
open Diagnostic

let source name content : Source.t =
  `String { name = Some name; content = Dedent.string content }
;;

let range ~source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)
;;

let pr_diagnostics diagnostics =
  let jsons = List.map diagnostics ~f:Grace_json_conv.json_of_diagnostic in
  Yojson.Safe.pretty_print Fmt.stdout (`List jsons)
;;

let pr_bad_diagnostics diagnostics =
  Fmt.(
    list
      ~sep:(fun ppf () -> pf ppf "@.@.")
      (fun ppf diagnostic ->
         try
           Yojson.Safe.pretty_print ppf (Grace_json_conv.json_of_diagnostic diagnostic)
         with
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
    [
      { "severity": "help", "message": "", "notes": [], "labels": [] },
      { "severity": "note", "message": "", "notes": [], "labels": [] },
      { "severity": "warning", "message": "", "notes": [], "labels": [] },
      { "severity": "error", "message": "", "notes": [], "labels": [] },
      { "severity": "bug", "message": "", "notes": [], "labels": [] }
    ]
    |}]
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
    [
      {
        "severity": "error",
        "message": "cannot borrow `v` as mutable more than once at a time",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "one_line.rs",
              "start": { "line": 3, "column": 12 },
              "end": { "line": 3, "column": 13 }
            },
            "priority": "primary",
            "message": "second mutable borrow occurs here"
          },
          {
            "range": {
              "source": "one_line.rs",
              "start": { "line": 3, "column": 5 },
              "end": { "line": 3, "column": 6 }
            },
            "priority": "secondary",
            "message": "first borrow later used by call"
          },
          {
            "range": {
              "source": "one_line.rs",
              "start": { "line": 3, "column": 7 },
              "end": { "line": 3, "column": 11 }
            },
            "priority": "secondary",
            "message": "first mutable borrow occurs here"
          }
        ]
      },
      {
        "severity": "error",
        "message": "aborting due to previous error",
        "notes": [
          "For more information about this error, try `rustc --explain`"
        ],
        "labels": []
      }
    ]
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
    [
      {
        "severity": "error",
        "message": "nested `impl Trait` is not allowed",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "nested_impl_trait.rs",
              "start": { "line": 5, "column": 56 },
              "end": { "line": 5, "column": 66 }
            },
            "priority": "primary",
            "message": "nested `impl Trait` here"
          },
          {
            "range": {
              "source": "nested_impl_trait.rs",
              "start": { "line": 5, "column": 46 },
              "end": { "line": 5, "column": 67 }
            },
            "priority": "secondary",
            "message": "outer `impl Trait`"
          }
        ]
      },
      {
        "severity": "error",
        "message": "the type placeholder `_` is not allowed within types on item signatures",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "typeck_type_placeholder_item.rs",
              "start": { "line": 1, "column": 18 },
              "end": { "line": 1, "column": 19 }
            },
            "priority": "primary",
            "message": "not allowed in type signatures"
          },
          {
            "range": {
              "source": "typeck_type_placeholder_item.rs",
              "start": { "line": 1, "column": 18 },
              "end": { "line": 1, "column": 19 }
            },
            "priority": "secondary",
            "message": "help: replace with the correct return type: `i32`"
          }
        ]
      },
      {
        "severity": "error",
        "message": "the type placeholder `_` is not allowed within types on item signatures",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "typeck_type_placeholder_item.rs",
              "start": { "line": 2, "column": 25 },
              "end": { "line": 2, "column": 26 }
            },
            "priority": "primary",
            "message": "not allowed in type signatures"
          },
          {
            "range": {
              "source": "typeck_type_placeholder_item.rs",
              "start": { "line": 2, "column": 28 },
              "end": { "line": 2, "column": 29 }
            },
            "priority": "primary",
            "message": "not allowed in type signatures"
          },
          {
            "range": {
              "source": "typeck_type_placeholder_item.rs",
              "start": { "line": 2, "column": 24 },
              "end": { "line": 2, "column": 30 }
            },
            "priority": "secondary",
            "message": "help: replace with the correct return type: `(i32, i32)`"
          }
        ]
      },
      {
        "severity": "error",
        "message": "`std::rc::Rc<()>` cannot be sent between threads safely",
        "notes": [
          "help: within `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`, the trait `std::marker::Send` is not implemented for `std::rc::Rc<()>`",
          "note: required because it appears within the type `Port<()>`",
          "note: required because it appears within the type `main::Foo`",
          "note: required because it appears within the type `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
        ],
        "labels": [
          {
            "range": {
              "source": "no_send_res_ports.rs",
              "start": { "line": 25, "column": 5 },
              "end": { "line": 25, "column": 18 }
            },
            "priority": "primary",
            "message": "`std::rc::Rc<()> cannot be sent between threads safely`"
          },
          {
            "range": {
              "source": "no_send_res_ports.rs",
              "start": { "line": 25, "column": 19 },
              "end": { "line": 28, "column": 7 }
            },
            "priority": "secondary",
            "message": "within this `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
          },
          {
            "range": {
              "source": "libstd/thread/mod.rs",
              "start": { "line": 5, "column": 8 },
              "end": { "line": 5, "column": 12 }
            },
            "priority": "secondary",
            "message": "required by this bound in `std::thread::spawn`"
          }
        ]
      },
      {
        "severity": "error",
        "message": "aborting due 5 previous errors",
        "notes": [
          "Some errors have detailed explanations: ...",
          "For more information about an error, try `rustc --explain`"
        ],
        "labels": []
      }
    ]
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
    [
      {
        "severity": "error",
        "message": "unexpected token",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "same_range",
              "start": { "line": 1, "column": 5 },
              "end": { "line": 1, "column": 6 }
            },
            "priority": "primary",
            "message": "Unexpected '{'"
          },
          {
            "range": {
              "source": "same_range",
              "start": { "line": 1, "column": 5 },
              "end": { "line": 1, "column": 6 }
            },
            "priority": "secondary",
            "message": "Expected '('"
          }
        ]
      }
    ]
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
    [
      {
        "severity": "error",
        "message": "match arms have incompatible types",
        "notes": [
          "expected `Result<ByteIndex, LineIndexOutOfBoundsError>`, found `LineIndexOutOfBoundsError`"
        ],
        "labels": [
          {
            "range": {
              "source": "file.rs",
              "start": { "line": 2, "column": 31 },
              "end": { "line": 2, "column": 76 }
            },
            "priority": "secondary",
            "message": "this is found to be of type `Result<ByteIndex, LineIndexOutOfBoundsError>`"
          },
          {
            "range": {
              "source": "file.rs",
              "start": { "line": 4, "column": 34 },
              "end": { "line": 7, "column": 14 }
            },
            "priority": "primary",
            "message": "expected enum `Result`, found struct `LineIndexOutOfBoundsError`"
          },
          {
            "range": {
              "source": "file.rs",
              "start": { "line": 1, "column": 9 },
              "end": { "line": 8, "column": 10 }
            },
            "priority": "secondary",
            "message": "`match` arms have incompatible types"
          },
          {
            "range": {
              "source": "file.rs",
              "start": { "line": 3, "column": 32 },
              "end": { "line": 3, "column": 60 }
            },
            "priority": "secondary",
            "message": "this is found to be of type `Result<ByteIndex, LineIndexOutOfBoundsError>`"
          }
        ]
      }
    ]
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
    [
      {
        "severity": "error",
        "message": "invalid ABI: found `路濫狼á́́`",
        "notes": [
          "valid ABIs:\n- aapcs\n- amdgpu-kernel\n- C\n- cdecl\n- efiapi\n- fastcall\n- msp430-interrupt\n- platform-intrinsic\n- ptx-kernel\n- Rust\n- rust-call\n- rust-intrinsic\n- stdcall\n- system\n- sysv64\n- thiscall\n- unadjusted\n- vectorcall\n- win64\n- x86-interrupt"
        ],
        "labels": [
          {
            "range": {
              "source": "unicode.rs",
              "start": { "line": 1, "column": 8 },
              "end": { "line": 1, "column": 16 }
            },
            "priority": "primary",
            "message": "invalid ABI"
          }
        ]
      }
    ]
    |}]
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

    Raised: (Invalid_argument "invalid UTF-8")
    |}]
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
    [
      {
        "severity": "note",
        "message": "empty range",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "hello.txt",
              "start": { "line": 1, "column": 7 },
              "end": { "line": 1, "column": 7 }
            },
            "priority": "primary",
            "message": "middle"
          }
        ]
      }
    ]
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
    {
      "severity": "error",
      "message": "err",
      "notes": [],
      "labels": [
        {
          "range": {
            "start": { "line": 1, "column": 1 },
            "end": { "line": 1, "column": 4 }
          },
          "priority": "primary",
          "message": "e1"
        },
        {
          "range": {
            "start": { "line": 3, "column": 1 },
            "end": { "line": 5, "column": 1 }
          },
          "priority": "secondary",
          "message": "e2"
        }
      ]
    }
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
    [
      {
        "severity": "error",
        "message": "syntax error",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "eof.ml",
              "start": { "line": 1, "column": 8 },
              "end": { "line": 1, "column": 8 }
            },
            "priority": "primary",
            "message": "Unexpected EOF here"
          }
        ]
      }
    ]
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
    [
      {
        "severity": "error",
        "message": "generic type variable `a` escapes its scope",
        "notes": [],
        "labels": [
          {
            "range": {
              "source": "rigid_variable_escape.ml",
              "start": { "line": 2, "column": 3 },
              "end": { "line": 3, "column": 17 }
            },
            "priority": "primary",
            "message": ""
          }
        ]
      }
    ]
    |}]
;;
