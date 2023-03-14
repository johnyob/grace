open Core
open Grace
open Grace_text
open Diagnostic

module Mark = struct
  module T = struct
    type t =
      { idx : int
      ; kind : [ `Top | `Vertical | `Bottom ]
      ; priority : Priority.t
      }
    [@@deriving equal, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Locus = struct
  type t =
    { file_name : string
    ; position : Position.t
    }
  [@@deriving sexp]
end

module Single_label = struct
  type t =
    { carets : Priority.t option list
    ; trailing_label : Label.t option
    }
  [@@deriving sexp]
end

module Caret_pointers = struct
  type t = (Span.Column_number.t * Priority.t) list
  (* invariant: sorted by start of span *) [@@deriving sexp]
end

module Hanging_label = struct
  type t =
    { pointers : Caret_pointers.t
    ; label : Label.t
    }
  [@@deriving sexp]
end

module Multi_label = struct
  type t =
    { mark_idx : int
    ; priority : Priority.t
    ; kind : [ `Top of Column_number.t | `Bottom of Column_number.t * Label.t ]
    }
  [@@deriving sexp]
end

module Content = struct
  type t =
    { line : Line_number.t
    ; source : (string * Priority.t option) list
    }
  [@@deriving sexp]
end

module Source_line = struct
  type t =
    | Content of Content.t
    | Single_label of Single_label.t
    | Caret_pointers of Caret_pointers.t
    | Hanging_label of Hanging_label.t
    | Multi_label of Multi_label.t
    | Break
  [@@deriving sexp]
end

module Raw_line = struct
  type t =
    | Locus of Locus.t
    | Title of
        { locus : Locus.t option
        ; title : Message.t
        }
    | Notes of Message.t list
  [@@deriving sexp]
end

module Line = struct
  type t =
    | Source of
        { marks : Mark.t list (* invariant: sorted on Mark.idx *)
        ; line : Source_line.t
        }
    | Raw of Raw_line.t
  [@@deriving sexp]
end

type t =
  { severity : Severity.t
  ; lines : Line.t list
  }
[@@deriving sexp]
