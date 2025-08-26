open! Import

type ('a, 'b) format = ('a, Format.formatter, unit, 'b) format4

module Severity = struct
  module T = struct
    type t =
      | Help
      | Note
      | Warning
      | Error
      | Bug
    [@@deriving sexp]

    let compare =
      (* [Stdlib.compare] will compare the underlying integer representations. *)
      (Stdlib.compare : t -> t -> int)
    ;;
  end

  include T
  include Comparable.Make (T)

  let to_string = function
    | Help -> "help"
    | Error -> "error"
    | Warning -> "warning"
    | Note -> "note"
    | Bug -> "bug"
  ;;

  let pp = Fmt.of_to_string to_string
end

module Priority = struct
  module T = struct
    type t =
      | Secondary
      | Primary
    [@@deriving sexp]

    let compare =
      (* [Stdlib.compare] will compare the underlying integer representations. *)
      (Stdlib.compare : t -> t -> int)
    ;;
  end

  include T
  include Comparable.Make (T)

  let is_primary = function
    | Primary -> true
    | Secondary -> false
  ;;

  let is_secondary = function
    | Primary -> false
    | Secondary -> true
  ;;

  let to_string = function
    | Primary -> "primary"
    | Secondary -> "secondary"
  ;;

  let pp = Fmt.of_to_string to_string
end

module Message = struct
  type t = Format.formatter -> unit

  let of_string str ppf =
    Fmt.(list ~sep:Format.pp_force_newline string) ppf @@ String.split_lines str
  ;;

  let to_string t =
    let buf = Buffer.create 512 in
    let ppf = Format.formatter_of_buffer buf in
    Format.pp_set_geometry ppf ~max_indent:2 ~margin:Format.pp_infinity;
    t ppf;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  ;;

  let sexp_of_t = sexp_of_t_of_to_string to_string
  let t_of_sexp = t_of_sexp_of_of_string of_string
  let create = of_string
  let createf = Format.dprintf
  let kcreatef = Format.kdprintf
  let pp ppf t = t ppf
end

module Label = struct
  type t =
    { range : Range.t
    ; priority : Priority.t
    ; message : Message.t
    }
  [@@deriving sexp]

  let create ~range ~priority message = { range; priority; message }
  let createf ~range ~priority fmt = fmt |> Message.kcreatef @@ create ~range ~priority

  let kcreatef ~range ~priority kont fmt =
    fmt |> Message.kcreatef @@ fun msg -> kont (create ~range ~priority msg)
  ;;

  let primary = create ~priority:Primary
  let primaryf = createf ~priority:Primary
  let kprimaryf = kcreatef ~priority:Primary
  let secondary = create ~priority:Secondary
  let secondaryf = createf ~priority:Secondary
  let ksecondaryf = kcreatef ~priority:Secondary
end

type 'code t =
  { severity : Severity.t
  ; message : Message.t
  ; code : 'code option
  ; labels : Label.t list
  ; notes : Message.t list
  }
[@@deriving sexp]

let create ?(notes = []) ?(labels = []) ?code severity message =
  { notes; labels; severity; message; code }
;;

let createf ?notes ?labels ?code severity fmt =
  fmt |> Message.kcreatef @@ create ?notes ?labels ?code severity
;;

let kcreatef ?notes ?labels ?code kont severity fmt =
  fmt |> Message.kcreatef @@ fun msg -> kont (create ?notes ?labels ?code severity msg)
;;
