open! Import

let foo = "foo\nbar\r\n\nbaz"
let files = Files.create ()
let foo_id = Files.add files "foo_test" foo
let pp_escaped_string ppf str = Fmt.pf ppf "%s" (String.escaped str)

let%expect_test _ =
  Fmt.pr "%s" (Files.name files foo_id);
  [%expect {| foo_test |}]
;;

let%expect_test _ =
  Fmt.pr "%a" pp_escaped_string (Files.source files foo_id);
  [%expect {| foo\nbar\r\n\nbaz |}]
;;

let%expect_test _ =
  (* Line index *)
  let byte_idxs = List.map ~f:Byte_index.create [ 0; 7; 8; 9; 11; 100 ] in
  Fmt.pr "source: %a@." pp_escaped_string foo;
  List.iter byte_idxs ~f:(fun idx ->
      Fmt.pr
        "@[<v>idx: %a@;Line.index idx: %a@]@."
        Byte_index.pp
        idx
        Line_index.pp
        (Files.Line.index files foo_id idx));
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
  let lines_starts = Files.Line.starts files foo_id in
  let last_line = Files.Line.last files foo_id in
  Fmt.pr
    "@[<v>source: %a@;line_starts: %a@;last: %a@]@."
    pp_escaped_string
    foo
    Fmt.(vbox (array ~sep:comma Byte_index.pp))
    lines_starts
    Line_index.pp
    last_line;
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
  let line_idxs = List.map ~f:Line_index.create [ 0; 1; 2; 3; 4 ] in
  Fmt.pr "source: %a@." pp_escaped_string foo;
  List.iter line_idxs ~f:(fun idx ->
      Fmt.pr
        "@[<v>idx: %a@;Line.range idx: %a@]@."
        Line_index.pp
        idx
        (Fmt.option Range.pp)
        (Files.Line.range files foo_id idx));
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
    Line.range idx: [10, 13)
    idx: 4
    Line.range idx: |}];
  (* Line slices (tests Line.range + Source.slice) *)
  Fmt.pr "source: %a@." pp_escaped_string foo;
  List.iter line_idxs ~f:(fun idx ->
      Fmt.pr
        "@[<v>idx: %a@;Line.slice idx: %a@]@."
        Line_index.pp
        idx
        Fmt.(option pp_escaped_string)
        (Files.Line.slice files foo_id idx));
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
    Line.slice idx: baz
    idx: 4
    Line.slice idx: |}]
;;

let%expect_test _ =
  (* Source range *)
  Fmt.pr
    "@[<v>source: %a@;source length: %d@;Source.range: %a@]"
    pp_escaped_string
    foo
    (String.length foo)
    Range.pp
    (Files.Source.range files foo_id);
  [%expect {|
    source: foo\nbar\r\n\nbaz
    source length: 13
    Source.range: [0, 13) |}]
;;
