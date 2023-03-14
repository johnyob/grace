open! Import

type reader =
  { id : int
  ; name : string option [@equal.ignore] [@compare.ignore] [@hash.ignore]
  ; length : int [@equal.ignore] [@compare.ignore] [@hash.ignore]
  ; unsafe_get : int -> char [@equal.ignore] [@compare.ignore] [@hash.ignore]
  }
[@@deriving equal, compare, hash, sexp]

let reader_name r = Option.value r.name ~default:(Int.to_string r.id)

type string_source =
  { name : string option
  ; content : string
  }
[@@deriving equal, compare, hash, sexp]

type t =
  [ `File of string
  | `String of string_source
  | `Reader of reader
  ]
[@@deriving equal, compare, hash, sexp]

let name = function
  | `File name -> Some name
  | `String { name; _ } -> name
  | `Reader reader -> Some (reader_name reader)
;;

let length = function
  | `File filename ->
    (try In_channel.(with_file filename ~f:length) |> Int64.to_int_exn with
     | _ -> invalid_argf "file size is larger than an OCaml 63-bit integer")
  | `String { content; _ } -> String.length content
  | `Reader { length; _ } -> length
;;
