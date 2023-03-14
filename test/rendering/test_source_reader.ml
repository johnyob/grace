open! Core
open! Grace
open! Grace_rendering

let () = Source_reader.init ()
let foo_content = "foo\nbar\r\n\nbaz"
let foo : Source.t = `String { name = Some "foo_test"; content = foo_content }
let foo_sd = Source_reader.open_source foo
let pp_escaped_string ppf str = Fmt.pf ppf "%s" (String.escaped str)

let pp_line_starts ppf (line_starts : Source_reader.Line_starts.t) =
  Fmt.pf ppf "@[<v>";
  for i = 0 to line_starts.length - 1 do
    if i <> 0 then Fmt.comma ppf ();
    Byte_index.pp ppf (line_starts.unsafe_get i)
  done;
  Fmt.pf ppf "@]"
;;

let%expect_test _ =
  Format.printf "%a" Format.(pp_print_option pp_print_string) (Source.name foo);
  [%expect {| foo_test |}]
;;

let%expect_test _ =
  (* Line index *)
  let byte_idxs = List.map ~f:Byte_index.of_int [ 0; 7; 8; 9; 11; 100 ] in
  Format.printf "source: %a@." pp_escaped_string foo_content;
  List.iter byte_idxs ~f:(fun idx ->
    Format.printf
      "@[<v>idx: %a@;Line.index idx: %a@]@."
      Byte_index.pp
      idx
      Line_index.pp
      (Source_reader.Line.of_byte_index foo_sd idx).idx);
  [%expect
    {|
    source: foo\nbar\r\n\nbaz
    idx: 0
    Line.index idx: 0
    idx: 7
    Line.index idx: 1
    idx: 8
    Line.index idx: 1
    idx: 9
    Line.index idx: 2
    idx: 11
    Line.index idx: 3
    idx: 100
    Line.index idx: 3 |}]
;;

let%expect_test _ =
  (* Line starts *)
  let lines_starts = Source_reader.line_starts foo_sd in
  let last_line = Source_reader.Line.last foo_sd in
  Fmt.pr
    "@[<v>source: %a@;line_starts: %a@;last: %a@]@."
    pp_escaped_string
    foo_content
    pp_line_starts
    lines_starts
    Line_index.pp
    last_line.idx;
  [%expect
    {|
    source: foo\nbar\r\n\nbaz
    line_starts: 0,
                 4,
                 9,
                 10
    last: 4 |}]
;;

let%expect_test _ =
  (* Line ranges *)
  let line_idxs = List.map ~f:Line_index.of_int [ 0; 1; 2; 3 ] in
  Fmt.pr "source: %a@." pp_escaped_string foo_content;
  List.iter line_idxs ~f:(fun idx ->
    Fmt.pr
      "@[<v>idx: %a@;Line.range idx: %a@]@."
      Line_index.pp
      idx
      Range.pp
      (Source_reader.Line.of_line_index foo_sd idx).range);
  [%expect
    {|
    source: foo\nbar\r\n\nbaz
    idx: 0
    Line.range idx: [0, 4)
    idx: 1
    Line.range idx: [4, 9)
    idx: 2
    Line.range idx: [9, 10)
    idx: 3
    Line.range idx: [10, 13) |}];
  (* Line slices (tests Line.range + Source.slice) *)
  Fmt.pr "source: %a@." pp_escaped_string foo_content;
  List.iter line_idxs ~f:(fun idx ->
    Fmt.pr
      "@[<v>idx: %a@;Line.slice idx: %a@]@."
      Line_index.pp
      idx
      pp_escaped_string
      Source_reader.Line.(of_line_index foo_sd idx |> slice ~sd:foo_sd));
  [%expect
    {|
    source: foo\nbar\r\n\nbaz
    idx: 0
    Line.slice idx: foo\n
    idx: 1
    Line.slice idx: bar\r\n
    idx: 2
    Line.slice idx: \n
    idx: 3
    Line.slice idx: baz |}]
;;

let%expect_test _ =
  (* Source length *)
  Fmt.pr
    "@[<v>source: %a@;source length: %d@;Source_reader.length: %d@]"
    pp_escaped_string
    foo_content
    (String.length foo_content)
    (Source_reader.length foo_sd);
  [%expect
    {|
    source: foo\nbar\r\n\nbaz
    source length: 13
    Source_reader.length: 13 |}]
;;

let () = Source_reader.clear ()
