open Core

(** Diagnostics and location associated metadata *)

(** {1 Location types}
    
    Index types are wrapper types that specify positions and ranges in a source file. 

    All indices are 0-indexed.
*)

module type Index = sig
  (** All indices are wrappers around 63-bit integers with the invariant 
      that an index [t] satisfies [t >= 0]. 
        
      Assumption : We won't be working with sources larger than ~1 PBs. *)
  type t = private int [@@deriving hash, sexp]

  include Comparable.S with type t := t
  include Invariant.S with type t := t

  (** [create n] creates the index [n]. 
      Raises an exception in [Error.t] that states "invariant" failed if [n < 0]. *)
  val create : int -> t

  val pp : t Fmt.t
  val ppd : t -> Fmt_doc.t

  (** [initial] is the initial index, namely [0]. *)
  val initial : t

  (** [t + off] adds the offset [off] to the index [t]. *)
  val ( + ) : t -> int -> t

  (** [t - off] subtracts the offset [off] from the index [t]. *)
  val ( - ) : t -> int -> t
end

module Line_index : Index
module Column_index : Index

module Byte_index : sig
  include Index

  val of_lex : Lexing.position -> t
end

module Range : sig
  module type S = sig
    type pos
    type t [@@deriving sexp]

    include Comparable.S with type t := t
    include Invariant.S with type t := t

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
    val create : pos -> pos -> t
    val initial : t
    val merge : t -> t -> t
    val are_disjoint : t -> t -> bool
    val contains : t -> pos -> bool
  end

  module Line_index : S with type pos := Line_index.t
  module Column_index : S with type pos := Column_index.t
  include S with type pos := Byte_index.t

  val of_string : string -> t
  val of_lex : Lexing.position -> Lexing.position -> t
end

(** {2 Location types for Text}
    
    Byte-oritented locations must be given proper handling for utf8 encodings.
*)

module Text : sig
  module type Number = sig
    type t = int [@@deriving hash, sexp]

    include Invariant.S with type t := t
    include Comparable.S with type t := t

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
    val to_string : t -> string
    val initial : t
  end

  module Line_number : sig
    include Number

    val of_index : Line_index.t -> t
  end

  module Column_number : sig
    include Number

    val of_index : Column_index.t -> line:string -> t
  end

  module Location : sig
    type t =
      { line : Line_number.t
      ; column : Column_number.t
      }
    [@@deriving sexp]

    include Comparable.S with type t := t
  end

  module Span : sig
    module type S = Range.S

    module Line_number : S with type pos := Line_number.t
    module Column_number : S with type pos := Column_number.t
    include S with type pos := Location.t
  end
end

(** {3 Files} *)

module File : sig
  module Id : Identifiable.S

  type t

  (** [create name source] *)
  val create : string -> string -> t

  val name : t -> string
  val line_start : t -> Line_index.t -> Byte_index.t
  val line_range : t -> Line_index.t -> Range.t
  val line_index : t -> Byte_index.t -> Line_index.t

  (* unicode functions *)
  val location : t -> Byte_index.t -> Text.Location.t
  val line_number : t -> Byte_index.t -> Text.Line_number.t
  val line_span : t -> Line_index.t -> Text.Span.t
  val source : t -> string
  val source_range : t -> Range.t
  val source_slice : t -> Range.t -> string

  module Cache : sig
    type file := t
    type t

    val create : unit -> t
    val add : t -> string -> string -> Id.t
    val find : t -> Id.t -> file
    val name : t -> Id.t -> string
    val line_start : t -> Id.t -> Line_index.t -> Byte_index.t
    val line_range : t -> Id.t -> Line_index.t -> Range.t
    val line_index : t -> Id.t -> Byte_index.t -> Line_index.t
    val location : t -> Id.t -> Byte_index.t -> Text.Location.t
    val line_number : t -> Id.t -> Byte_index.t -> Text.Line_number.t
    val line_span : t -> Id.t -> Line_index.t -> Text.Span.t
    val source : t -> Id.t -> string
    val source_range : t -> Id.t -> Range.t
    val source_slice : t -> Id.t -> Range.t -> string
  end
end

(** {4 Diagnostics} *)

module Diagnostic : sig
  module Severity : sig
    type t =
      | Help
      | Note
      | Warning
      | Error
      | Bug
    [@@deriving equal, compare, sexp]

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
  end

  module Priority : sig
    type t =
      | Primary
      | Secondary
    [@@deriving equal, compare, sexp]

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
    val is_primary : t -> bool
    val is_secondary : t -> bool
  end

  module Message : sig
    type t = Formatter.t -> unit [@@deriving sexp]

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
  end

  module Label : sig
    type t =
      { id : File.Id.t
      ; range : Range.t
      ; priority : Priority.t
      ; message : Message.t
      }
    [@@deriving sexp]

    val primary : id:File.Id.t -> range:Range.t -> Message.t -> t
    val secondary : id:File.Id.t -> range:Range.t -> Message.t -> t
  end

  type t =
    { severity : Severity.t
    ; message : Message.t
    ; labels : Label.t list
    ; notes : Message.t list
    }
  [@@deriving sexp]
end
