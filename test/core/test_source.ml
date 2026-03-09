open! Import

let%expect_test "Source.length raises Sys_error for empty filename" =
  let source : Source.t = `File "" in
  fail (fun () -> ignore (Source.length source : int));
  [%expect
    {|
    Test raised error as expected.
    Error: (Sys_error ": No such file or directory")
    |}]
;;

let%expect_test "Source.length raises Sys_error for non-existent file" =
  let source : Source.t = `File "/nonexistent/file/path/that/does/not/exist.txt" in
  fail (fun () -> ignore (Source.length source : int));
  [%expect
    {|
    Test raised error as expected.
    Error: (Sys_error
     "/nonexistent/file/path/that/does/not/exist.txt: No such file or directory")
    |}]
;;

let with_temp_file f =
  let temp_file = Filename_unix.temp_file "test_source" ".txt" in
  Fun.protect (fun () -> f temp_file) ~finally:(fun () -> Sys_unix.remove temp_file)
;;

let%expect_test "Source.length raises Sys_error for permission denied" =
  with_temp_file
  @@ fun temp_file ->
  Out_channel.write_all temp_file ~data:"test content";
  Core_unix.chmod temp_file ~perm:0o000;
  let source : Source.t = `File temp_file in
  fail (fun () ->
    try Source.length source with
    | Sys_error err ->
      (* The error message is of the form: <FILENAME>: Permission denied
         For determinism, we strip the filename from the error message.  *)
      (match String.lsplit2 err ~on:':' with
       | Some (_filename, msg) ->
         assert (String.is_substring msg ~substring:"Permission denied");
         raise (Sys_error msg)
       | None -> raise (Sys_error err)));
  [%expect
    {|
    Test raised error as expected.
    Error: (Sys_error " Permission denied")
    |}]
;;

let%test_unit "Source.length for String sources" =
  let content = "Hello, World!" in
  let source : Source.t = `String { name = Some "test"; content } in
  let length = Source.length source in
  [%test_eq: int] length (String.length content)
;;

let%test_unit "Source.length for Reader sources" =
  let length = 42 in
  let source : Source.t =
    `Reader { id = 0; name = Some "test"; length; unsafe_get = (fun _ -> 'x') }
  in
  let actual_length = Source.length source in
  [%test_eq: int] actual_length length
;;

let%test_unit "Source.length for actual files" =
  with_temp_file
  @@ fun temp_file ->
  let content = "This is a test file with some content." in
  Out_channel.write_all temp_file ~data:content;
  let source : Source.t = `File temp_file in
  let actual_length = Source.length source in
  let expected_length = String.length content in
  [%test_eq: int] actual_length expected_length
;;
