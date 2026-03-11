(** Convert a [Grace.Diagnostic.t] to a JSON representation.
  The resulting JSON has the following shape:
  {[
    {
      "severity": "help" | "error" | "warning" | "note" | "bug",
      "message": string,
      "error_code": string?,
      "labels": [
        {
          "range": {
            "source": string?,
            "start": { "line": int, "column": int },
            "end": { "line": int, "column": int }
          },
          "priority": "primary" | "secondary",
          "message": string
        }*
      ],
      "notes": [ string* ]
    }
  ]}
*)
val json_of_diagnostic
  :  ?code_to_string:('code -> string)
  -> 'code Grace.Diagnostic.t
  -> Yojson.Safe.t
