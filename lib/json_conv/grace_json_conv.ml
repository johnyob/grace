open Grace
open Grace_std

let json_of_byte_index ~in_ (idx : Byte_index.t) =
  let open Grace_source_reader.Line in
  let line = of_byte_index in_ idx in
  let col = column_offset ~in_ line idx in
  let line_json = `Int ((line.idx :> int) + 1) in
  let column_json = `Int col in
  `Assoc [ "line", line_json; "column", column_json ]
;;

let json_of_range (r : Range.t) : Yojson.Basic.t =
  let sd = Grace_source_reader.open_source (Range.source r) in
  let vals =
    [ "start", json_of_byte_index ~in_:sd (Range.start r)
    ; "end", json_of_byte_index ~in_:sd (Range.stop r)
    ]
  in
  match Source.name (Range.source r) with
  | Some name -> `Assoc (("source", `String name) :: vals)
  | None -> `Assoc vals
;;

let json_of_message (message : Diagnostic.Message.t) : Yojson.Basic.t =
  `String (Diagnostic.Message.to_string message)
;;

let json_of_label ({ range; priority; message } : Diagnostic.Label.t) : Yojson.Basic.t =
  `Assoc
    Diagnostic.
      [ "range", json_of_range range
      ; "priority", `String (Priority.to_string priority)
      ; "message", json_of_message message
      ]
;;

let json_of_diagnostic
      ?code_to_string
      ({ severity; message; notes; labels; code } : 'a Diagnostic.t)
  : Yojson.Basic.t
  =
  Grace_source_reader.with_reader
  @@ fun () ->
  let vals =
    Diagnostic.
      [ "severity", `String (Severity.to_string severity)
      ; "message", json_of_message message
      ; "notes", `List (List.map notes ~f:json_of_message)
      ; "labels", `List (List.map labels ~f:json_of_label)
      ]
  in
  match code_to_string, code with
  | Some f, Some c -> `Assoc (("error_code", `String (f c)) :: vals)
  | _ -> `Assoc vals
;;
