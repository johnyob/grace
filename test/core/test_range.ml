open! Import

let source_length = 40

let source : Source.t =
  `Reader
    { id = 0; name = Some "test"; length = source_length; unsafe_get = (fun _ -> 'a') }
;;

let range start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)
;;

let range_generator =
  Quickcheck.Generator.(
    let open Let_syntax in
    let%bind idx = small_non_negative_int in
    let%map off = Int.gen_incl 0 (source_length - idx) in
    range idx (idx + off))
;;

let%expect_test _ =
  fail (fun () -> Byte_index.of_int (-1));
  [%expect
    {|
    Test raised error as expected.
    Error: ("invariant failed" lib/core/index.ml:28:24
     (exn "Assert_failure lib/core/index.ml:28:55") -1) |}];
  let print ~op ~op_name idx off =
    Fmt.pr
      "@[<v>idx: %a@;off: %d@;idx %s off: %a@]@."
      Byte_index.pp
      idx
      off
      op_name
      Byte_index.pp
      (op idx off)
  in
  (* Add offset *)
  let idx = Byte_index.of_int 20 in
  List.iter [ 2; 10; -2 ] ~f:(fun off -> print ~op:Byte_index.add ~op_name:"+" idx off);
  [%expect
    {|
    idx: 20
    off: 2
    idx + off: 22
    idx: 20
    off: 10
    idx + off: 30
    idx: 20
    off: -2
    idx + off: 18 |}];
  (* Subtract offset *)
  let idx = Byte_index.of_int 10 in
  List.iter [ 8; -3 ] ~f:(fun off -> print ~op:Byte_index.sub ~op_name:"-" idx off);
  [%expect
    {|
    idx: 10
    off: 8
    idx - off: 2
    idx: 10
    off: -3
    idx - off: 13 |}];
  (* Add -ve offset invaliding [idx > 0] invariant *)
  fail (fun () ->
    let idx = Byte_index.of_int 2 in
    let off = -10 in
    print ~op:Byte_index.add ~op_name:"+" idx off);
  [%expect
    {|
    Test raised error as expected.
    Error: ("invariant failed" lib/core/index.ml:28:24
     (exn "Assert_failure lib/core/index.ml:28:55") -8) |}];
  (* Subtract +ve offset invaliding [idx > 0] invariant *)
  fail (fun () ->
    let idx = Byte_index.of_int 10 in
    let off = 20 in
    print ~op:Byte_index.sub ~op_name:"-" idx off);
  [%expect
    {|
    Test raised error as expected.
    Error: ("invariant failed" lib/core/index.ml:28:24
     (exn "Assert_failure lib/core/index.ml:28:55") -10) |}]
;;

let%test_unit _ =
  (* Identity *)
  Quickcheck.test range_generator ~f:(fun a -> assert (Range.(merge a a = a)))
;;

let%test_unit _ =
  (* Commute *)
  Quickcheck.test
    Quickcheck.Generator.(tuple2 range_generator range_generator)
    ~f:(fun (a, b) -> assert (Range.(merge a b = merge b a)))
;;

let%expect_test _ =
  let print a b =
    Fmt.pr
      "@[<v>a: %a@;b: %a@;Range.merge a b: %a@;Range.inter a b: %a@]@."
      Range.pp
      a
      Range.pp
      b
      Range.pp
      (Range.merge a b)
      Range.pp
      (Range.inter a b)
  in
  (* Overlapping *)
  let a = range 1 5 in
  let b = range 3 10 in
  print a b;
  [%expect
    {|
    a: [1, 5)
    b: [3, 10)
    Range.merge a b: [1, 10)
    Range.inter a b: [3, 5) |}];
  (* Subset *)
  let a = range 1 10 in
  let b = range 3 7 in
  print a b;
  [%expect
    {|
    a: [1, 10)
    b: [3, 7)
    Range.merge a b: [1, 10)
    Range.inter a b: [3, 7) |}];
  (* Disjoint *)
  let a = range 1 10 in
  let b = range 10 20 in
  print a b;
  [%expect
    {|
    a: [1, 10)
    b: [10, 20)
    Range.merge a b: [1, 20)
    Range.inter a b: [10, 10) |}]
;;

let%expect_test _ =
  let print a b =
    Fmt.pr
      "@[<v>a: %a@;b: %a@;Range.are_disjoint a b: %b@]@."
      Range.pp
      a
      Range.pp
      b
      (Range.are_disjoint a b)
  in
  (* Overlapping *)
  let a = range 1 5 in
  let b = range 3 10 in
  print a b;
  [%expect {|
    a: [1, 5)
    b: [3, 10)
    Range.are_disjoint a b: false |}];
  (* Subset *)
  let a = range 1 10 in
  let b = range 3 7 in
  print a b;
  [%expect {|
    a: [1, 10)
    b: [3, 7)
    Range.are_disjoint a b: false |}];
  (* Disjoint *)
  let a = range 1 5 in
  let b = range 10 20 in
  print a b;
  [%expect {|
    a: [1, 5)
    b: [10, 20)
    Range.are_disjoint a b: true |}];
  (* Off by one *)
  let b = range 5 10 in
  print a b;
  [%expect {|
    a: [1, 5)
    b: [5, 10)
    Range.are_disjoint a b: true |}];
  let b = range 0 1 in
  print a b;
  [%expect {|
    a: [1, 5)
    b: [0, 1)
    Range.are_disjoint a b: true |}]
;;
