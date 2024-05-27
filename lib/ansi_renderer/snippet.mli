open! Core
open! Grace
open! Diagnostic

(** [Number]s are indexes (with different initial constraints) *)
module type Number = Index

module Line_number : sig
  include Number

  val of_line_index : Line_index.t -> t
end

module Column_number : Number

module Multi_line_label : sig
  module Id : Identifiable.S

  (** A multi-line-label is defined by a [Top] and [Bottom] marker within a {!type:source}. *)
  type t =
    | Top of
        { id : Id.t (** The unique identifier. *)
        ; start : Column_number.t (** The column number of the start of the label. *)
        ; priority : Priority.t (** The priority of the label. *)
        }
    | Bottom of
        { id : Id.t (** The unique identifier. *)
        ; stop : Column_number.t (** The column number of the end of the label *)
        ; priority : Priority.t (** The priority of the label. *)
        ; label : Message.t (** The message of the label. *)
        }
  [@@deriving sexp]
end

module Line : sig
  (** A semantic tag *)
  type stag =
    { priority : Priority.t
    ; inline_labels : (Priority.t * Message.t) list
    }

  (** A segment is a string with an optional semantic tag. *)
  and segment =
    { content : string
    ; length : int
    ; stag : stag option
    }

  (** A line is a list of {!type:segment}s with {{!type:Multi_line_label.t} multi-line label}s. *)
  and t =
    { segments : segment list
    ; multi_line_labels : Multi_line_label.t list
    ; margin_length : int
    }
  [@@deriving sexp]
end

(** A block is a list of consecutive lines. *)
type block =
  { start : Line_index.t
  (** The starting {{!type:Line_index.t} line index} of the block. *)
  ; lines : Line.t list (** The {{!type:Line.t} line}s in the block. *)
  }

(** A source consists of multiple blocks within the same {{!type:Source.t} source}. *)
and source =
  { source : Source.t (** The source. *)
  ; locus : locus (** The 'locus' position in the file. *)
  ; blocks : block list
  (** The list of {!type:block}s. The blocks are non-overlapping and sorted. *)
  }

and locus = Line_number.t * Column_number.t

and sources =
  | Rich of source list
  | Compact of (Source.t * locus) list

(** The type of a snippet, an internal representation of a rendered diagnostic. *)
and t =
  { severity : Severity.t (** The severity of the diagnostic. *)
  ; message : Message.t (** The primary message of the diagnostic. *)
  ; sources : sources (** The sources associated with the diagnostic. *)
  ; notes : Message.t list (** The notes of the diagnostic. *)
  }
[@@deriving sexp]

(** [of_diagnostic diagnostic] returns the ('rich') snippet compiled from the [diagnostic]. *)
val of_diagnostic : Diagnostic.t -> t

(** [compact_of_diagnostic diagnostic] returns the 'compact' snippet compiled from the [diagnostic]. *)
val compact_of_diagnostic : Diagnostic.t -> t
