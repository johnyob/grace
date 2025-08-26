open! Import

module Reader = struct
  module T = struct
    type t =
      { id : int
      ; name : string option [@ignore]
      ; length : int [@ignore]
      ; unsafe_get : int -> char [@ignore]
      }
    [@@deriving sexp]

    let compare t1 t2 = if phys_equal t1 t2 then 0 else Int.compare t1.id t2.id
  end

  include T
  include Comparable.Make (T)

  let name r = Option.value r.name ~default:(Int.to_string r.id)
end

type string_source =
  { name : string option
  ; content : string
  }
[@@deriving sexp]

let compare_string_source str_src1 str_src2 =
  if phys_equal str_src1 str_src2
  then 0
  else (
    match Option.compare String.compare str_src1.name str_src2.name with
    | 0 -> String.compare str_src1.content str_src2.content
    | n -> n)
;;

module T = struct
  type t =
    [ `File of string
    | `String of string_source
    | `Reader of Reader.t
    ]
  [@@deriving sexp]

  let compare t1 t2 =
    if phys_equal t1 t2
    then 0
    else (
      match t1, t2 with
      | `File fname1, `File fname2 -> String.compare fname1 fname2
      | `String str_src1, `String str_src2 -> compare_string_source str_src1 str_src2
      | `Reader rd1, `Reader rd2 -> Reader.compare rd1 rd2
      | _, _ ->
        (* Compare the tags *)
        Stdlib.compare t1 t2)
  ;;
end

include T
include Comparable.Make (T)

let name = function
  | `File name -> Some name
  | `String { name; _ } -> name
  | `Reader reader -> Some (Reader.name reader)
;;

let length = function
  | `File filename ->
    (try In_channel.(with_open_bin filename length) |> Int64.to_int with
     | _ -> invalid_argf "file size is larger than an OCaml 63-bit integer")
  | `String { content; _ } -> String.length content
  | `Reader { Reader.length; _ } -> length
;;
