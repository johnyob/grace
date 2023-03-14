open Core
open Source

module Severity = struct
  type t =
    | Help
    | Note
    | Warning
    | Error
    | Bug
  [@@deriving equal, compare, sexp]

  let pp ppf t =
    match t with
    | Help -> Fmt.pf ppf "help"
    | Error -> Fmt.pf ppf "error"
    | Warning -> Fmt.pf ppf "warning"
    | Note -> Fmt.pf ppf "note"
    | Bug -> Fmt.pf ppf "bug"
  ;;

  let ppd = Fmt_doc.of_pp pp
end

module Priority = struct
  type t =
    | Primary
    | Secondary
  [@@deriving equal, sexp]

  (* Ensures [compare Primary Secondary > 0] *)
  let compare t1 t2 =
    match t1, t2 with
    | Primary, Primary -> 0
    | Primary, Secondary -> 1
    | Secondary, Secondary -> 0
    | Secondary, Primary -> -1
  ;;

  let is_primary = function
    | Primary -> true
    | Secondary -> false
  ;;

  let is_secondary = function
    | Primary -> false
    | Secondary -> true
  ;;

  let pp ppf t =
    match t with
    | Primary -> Fmt.pf ppf "primary"
    | Secondary -> Fmt.pf ppf "secondary"
  ;;

  let ppd = Fmt_doc.of_pp pp
end

module Message = struct
  type t = Formatter.t -> unit

  let pp ppf t = t ppf
  let ppd = Fmt_doc.of_pp pp

  let to_string t =
    t Format.str_formatter;
    Format.flush_str_formatter ()
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
  let t_of_sexp sexp : t = fun ppf -> Fmt.string ppf (Sexp.to_string sexp)
end

module Label = struct
  type t =
    { id : File.Id.t
    ; range : Range.t
    ; priority : Priority.t
    ; message : Message.t
    }
  [@@deriving sexp]

  let primary ~id ~range message = { id; range; message; priority = Primary }
  let secondary ~id ~range message = { id; range; message; priority = Secondary }
end

type t =
  { severity : Severity.t
  ; message : Message.t
  ; labels : Label.t list
  ; notes : Message.t list
  }
[@@deriving sexp]
