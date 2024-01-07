open! Core
open! Grace
open! Diagnostic

module Multi_line_label : sig
  module Id : Identifiable.S

  (** A multi-line-label is defined by a [Top] and [Bottom] marker within a {!type:source}. *)
  type t =
    | Top of
        { id : Id.t (** The unique identifier. *)
        ; start : Byte_index.t (** The starting byte index of the label. *)
        ; priority : Priority.t (** The priority of the label. *)
        }
    | Bottom of
        { id : Id.t (** The unique identifier. *)
        ; stop : Byte_index.t (** The starting byte index of the label. *)
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
    ; range : Range.t
    ; stag : stag option
    }

  (** A line is a list of {!type:segment}s with {{!type:Multi_line_label.t} multi-line label}s. *)
  and t =
    { start : Byte_index.t
    ; segments : segment list
    ; multi_line_labels : Multi_line_label.t list
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
  ; locus : Line_index.t * Column_index.t (** The 'locus' position. *)
  ; labels : Label.t list (** The labels within the source. *)
  ; blocks : block list
  (** The list of {!type:block}s. The blocks are non-overlapping and sorted. *)
  }

(** The type of a snippet, an internal representation of a rendered diagnostic. *)
and t =
  { severity : Severity.t (** The severity of the diagnostic. *)
  ; message : Message.t (** The primary message of the diagnostic. *)
  ; sources : source list (** The sources associated with the diagnostic. *)
  ; notes : Message.t list (** The notes of the diagnostic. *)
  }
[@@deriving sexp]

(** [of_diagnostic diagnostic] returns the snippet compiled from the [diagnostic]. *)
val of_diagnostic : Diagnostic.t -> t
