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
end

module Priority = struct
  type t =
    | Primary
    | Secondary
  [@@deriving equal, compare, sexp]

  let is_primary = function
    | Primary -> true
    | Secondary -> false
  ;;

  let is_secondary = function
    | Primary -> false
    | Secondary -> true
  ;;
end

module Message = struct
  type t = Formatter.t -> unit

  let pp ppf t = t ppf
end

module Label = struct
  type t =
    { id : File.Id.t
    ; range : Range.t
    ; priority : Priority.t
    ; message : Message.t
    }

  let primary ~id ~range message = { id; range; message; priority = Primary }
  let secondary ~id ~range message = { id; range; message; priority = Secondary }
end

type t =
  { severity : Severity.t
  ; message : Message.t
  ; labels : Label.t list
  ; notes : Message.t list
  }
