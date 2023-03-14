open! Core
open! Grace
open Diagnostic

let range start stop = Range.create (Byte_index.create start) (Byte_index.create stop)

let print_rich_diagnostics ?debug ~files diagnostics =
  (* Disable colors for tests (since expect tests don't support ANSI colors) *)
  let config = Grace_rendering.Config.{ default with color = false } in
  Fmt_doc.(
    concat
      ~sep:newline
      (List.map diagnostics ~f:(Grace_rendering.ppd_rich ?debug ~config ~files))
    ++ newline)
  |> Fmt_doc.render Fmt.stdout
;;

(* Taken from https://github.com/brendanzab/codespan/blob/master/codespan-reporting/tests/term.rs *)

let%expect_test "empty" =
  let files = Files.create () in
  let diagnostics =
    let empty severity =
      Diagnostic.
        { severity; message = (fun ppf -> Fmt.pf ppf ""); labels = []; notes = [] }
    in
    List.map ~f:empty Severity.[ Help; Note; Warning; Error; Bug ]
  in
  print_rich_diagnostics ~files diagnostics;
  [%expect {|
    help:

    note:

    warning:

    error:

    bug: |}]
;;

let%expect_test "same_line" =
  let files = Files.create () in
  let one_line_id =
    Files.add
      files
      "one_line.rs"
      (String.lstrip
         {|
fn main() {
    let mut v = vec![Some("foo"), Some("bar")];
    v.push(v.pop().unwrap());
}
  |})
  in
  let diagnostics =
    Diagnostic.
      [ { severity = Error
        ; message =
            (fun ppf ->
              Fmt.pf ppf "cannot borrow `v` as mutable more than once at a time")
        ; labels =
            [ Label.primary ~id:one_line_id ~range:(range 71 72)
              @@ Fmt.fmt "second mutable borrow occurs here"
            ; Label.secondary ~id:one_line_id ~range:(range 64 65)
              @@ Fmt.fmt "first borrow later used by call"
            ; Label.secondary ~id:one_line_id ~range:(range 66 70)
              @@ Fmt.fmt "first mutable borrow occurs here"
            ]
        ; notes = []
        }
      ; { severity = Error
        ; message = (fun ppf -> Fmt.pf ppf "aborting due to previous error")
        ; labels = []
        ; notes =
            [ Fmt.fmt "For more information about this error, try `rustc --explain`" ]
        }
      ]
  in
  print_rich_diagnostics ~debug:false ~files diagnostics;
  [%expect
    {|
    error: cannot borrow `v` as mutable more than once at a time
        ┌─ one_line.rs:3:12
      3 │      v.push(v.pop().unwrap());
        │      - ---- ^ second mutable borrow occurs here
        │      │ │
        │      │ first mutable borrow occurs here
        │      first borrow later used by call

    error: aborting due to previous error
        = For more information about this error, try `rustc --explain` |}]
;;

(* observations:
    - strange space (might be introduced by multilabels) 
*)

let%expect_test "overlapping" =
  let files = Files.create () in
  let file_id1 =
    Files.add
      files
      "nested_impl_trait.rs"
      (String.lstrip
         {|
use std::fmt::Debug;

fn fine(x: impl Into<u32>) -> impl Into<u32> { x }

fn bad_in_ret_position(x: impl Into<u32>) -> impl Into<impl Debug> { x }
|})
  in
  let file_id2 =
    Files.add
      files
      "typeck_type_placeholder_item.rs"
      (String.lstrip
         {|
fn fn_test1() -> _ { 5 }
fn fn_test2(x: i32) -> (_, _) { (x, x) }  
|})
  in
  let file_id3 =
    Files.add
      files
      "libstd/thread/mod.rs"
      (String.lstrip
         {|
#[stable(feature = "rust1", since = "1.0.0")]
pub fn spawn<F, T>(self, f: F) -> io::Result<JoinHandle<T>>
where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
{
    unsafe { self.spawn_unchecked(f) }
}
|})
  in
  let file_id4 =
    Files.add
      files
      "no_send_res_ports.rs"
      (String.lstrip
         {|
use std::thread;
use std::rc::Rc;

#[derive(Debug)]
struct Port<T>(Rc<T>);

fn main() {
    #[derive(Debug)]
    struct Foo {
        _x: Port<()>,
    }

    impl Drop for Foo {
        fn drop(&mut self) {}
    }

    fn foo(x: Port<()>) -> Foo {
        Foo {
            _x: x
        }
    }

    let x = foo(Port(Rc::new(())));
    
    thread::spawn(move|| {
        let y = x;
        println!("{:?}", y);
    });
}
|})
  in
  let diagnostics =
    Diagnostic.
      [ { severity = Error
        ; message = Fmt.fmt "nested `impl Trait` is not allowed"
        ; labels =
            Label.
              [ primary ~id:file_id1 ~range:(range 129 139)
                @@ Fmt.fmt "nested `impl Trait` here"
              ; secondary ~id:file_id1 ~range:(range 119 140)
                @@ Fmt.fmt "outer `impl Trait`"
              ]
        ; notes = []
        }
      ; { severity = Error
        ; message =
            Fmt.fmt
              "the type placeholder `_` is not allowed within types on item signatures"
        ; labels =
            Label.
              [ primary ~id:file_id2 ~range:(range 17 18)
                @@ Fmt.fmt "not allowed in type signatures"
              ; secondary ~id:file_id2 ~range:(range 17 18)
                @@ Fmt.fmt "help: replace with the correct return type: `i32`"
              ]
        ; notes = []
        }
      ; { severity = Error
        ; message =
            Fmt.fmt
              "the type placeholder `_` is not allowed within types on item signatures"
        ; labels =
            Label.
              [ primary ~id:file_id2 ~range:(range 49 50)
                @@ Fmt.fmt "not allowed in type signatures"
              ; primary ~id:file_id2 ~range:(range 52 53)
                @@ Fmt.fmt "not allowed in type signatures"
              ; secondary ~id:file_id2 ~range:(range 48 54)
                @@ Fmt.fmt "help: replace with the correct return type: `(i32, i32)`"
              ]
        ; notes = []
        }
      ; { severity = Error
        ; message = Fmt.fmt "`std::rc::Rc<()>` cannot be sent between threads safely"
        ; labels =
            Label.
              [ primary ~id:file_id4 ~range:(range 339 352)
                @@ Fmt.fmt "`std::rc::Rc<()> cannot be sent between threads safely`"
              ; secondary ~id:file_id4 ~range:(range 357 420)
                @@ Fmt.fmt
                     "within this `[closure@no_send_res_ports.rs:29:19: 33:6 \
                      x:main::Foo]`"
              ; secondary ~id:file_id3 ~range:(range 141 145)
                @@ Fmt.fmt "required by this bound in `std::thread::spawn`"
              ]
        ; notes =
            Fmt.
              [ fmt
                  "help: within `[closure@no_send_res_ports.rs:29:19: 33:6 \
                   x:main::Foo]`, the trait `std::marker::Send` is not implemented for \
                   `std::rc::Rc<()>`"
              ; fmt "note: required because it appears within the type `Port<()>`"
              ; fmt "note: required because it appears within the type `main::Foo`"
              ; fmt
                  "note: required because it appears within the type \
                   `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`"
              ]
        }
      ; { severity = Error
        ; message = Fmt.fmt "aborting due 5 previous errors"
        ; labels = []
        ; notes =
            Fmt.
              [ fmt "Some errors have detailed explanations: ..."
              ; fmt "For more information about an error, try `rustc --explain`"
              ]
        }
      ]
  in
  print_rich_diagnostics ~files diagnostics;
  [%expect
    {|
    error: nested `impl Trait` is not allowed
        ┌─ nested_impl_trait.rs:5:56
      5 │  fn bad_in_ret_position(x: impl Into<u32>) -> impl Into<impl Debug> { x }
        │                                               ----------^^^^^^^^^^-
        │                                               │         │
        │                                               │         nested `impl Trait` here
        │                                               outer `impl Trait`

    error: the type placeholder `_` is not allowed within types on item signatures
        ┌─ typeck_type_placeholder_item.rs:1:18
      1 │  fn fn_test1() -> _ { 5 }
        │                   ^
        │                   │
        │                   │help: replace with the correct return type: `i32`
        │                   not allowed in type signatures

    error: the type placeholder `_` is not allowed within types on item signatures
        ┌─ typeck_type_placeholder_item.rs:2:28
      2 │  fn fn_test2(x: i32) -> (_, _) { (x, x) }
        │                         -^--^-
        │                         ││  │
        │                         ││  not allowed in type signatures
        │                         │not allowed in type signatures
        │                         help: replace with the correct return type: `(i32, i32)`

    error: `std::rc::Rc<()>` cannot be sent between threads safely
        ┌─ libstd/thread/mod.rs:5:8
      5 │       F: Send + 'static,
        │          ---- required by this bound in `std::thread::spawn`
        ┌─ no_send_res_ports.rs:25:1
     24 │
     25 │       thread::spawn(move|| {
        │ │ ^^^^^^^^^^^^^ `std::rc::Rc<()> cannot be sent between threads safely`
        │ ╭───────────────────'
     26 │ │         let y = x;
     27 │ │         println!("{:?}", y);
     28 │ │     });
        │ ╰───────' within this `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`
     29 │   }
        = help: within `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`, the trait `std::marker::Send` is not implemented for `std::rc::Rc<()>`
        = note: required because it appears within the type `Port<()>`
        = note: required because it appears within the type `main::Foo`
        = note: required because it appears within the type `[closure@no_send_res_ports.rs:29:19: 33:6 x:main::Foo]`
    error: aborting due 5 previous errors
        = Some errors have detailed explanations: ...
        = For more information about an error, try `rustc --explain` |}]
;;

(* observations:
    ordering of overlapping labels is strange (probably due to some of the sorting we do)    
    we add extra caret pointers for hanging labels in the same position. 
    mutlilabel top "│"
*)