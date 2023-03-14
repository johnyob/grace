open Core

(** Diagnostics and location associated metadata *)

(** {1 Indexes}
    
    Index types are integer types that specify byte-indexed positions in a source file. 
    All indices are 0-indexed and positive. 

    Grace uses byte-indexed positions are they are the most portable location information.
    Alternatives include:
    - Character positions
    - Column and line numbers

    Neither alternatives are well-defined without contextual information (encodings, etc). 
*)

module type Index = sig
  (** All indices are integers with the invariant that an index [t] satisfies [t >= 0]. *)
  type t = private int [@@deriving hash, sexp]

  include Comparable.S with type t := t
  include Invariant.S with type t := t

  (** [create n] creates the index [n]. 

      @raise an exception if [n < 0]. *)
  val create : int -> t

  val pp : t Fmt.t
  val ppd : t -> Fmt_doc.t

  (** [initial] is the initial index, namely [0]. *)
  val initial : t

  (** [t + off] adds the offset [off] to the index [t]. *)
  val ( + ) : t -> int -> t

  (** [t - off] subtracts the offset [off] from the index [t]. *)
  val ( - ) : t -> int -> t

  (** [t1 -. t2] returns the (potentially negative) offset between [t1] and [t2]. *)
  val ( -. ) : t -> t -> int
end

module Line_index : Index
module Column_index : Index

module Byte_index : sig
  include Index

  (** [of_lex lex_pos] returns the byte index from the lexing position [lex_pos]. 
      Equivalent to [create (lex_pos.pos_cnum)]. *)
  val of_lex : Lexing.position -> t
end

(** {2 Ranges}

    Grace's {e ranges} are opaque byte index internals of the form {e \[start, stop)} 
    (not including the byte at the [stop] index).  

    Grace offers three kinds of ranges: 
    - Line index ranges
    - Column index ranges, so-called "sub ranges" within a line
    - Byte ranges

    These three types of ranges offer the precision often required when dealing with 
    ranges within a file.
*)
module Range : sig
  module type Position = sig
    type t [@@deriving sexp, compare]

    val pp : t Fmt.t
    val initial : t
  end

  module type S = sig
    (** The type of positions *)
    type position

    (** The abstract type of ranges *)
    type t [@@deriving sexp]

    include Comparable.S with type t := t
    include Invariant.S with type t := t

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t

    (** [create start stop] returns the range {e \[start, stop)}.
        
        @raise an exception if [stop] is before [start]. *)
    val create : position -> position -> t

    (** [start t] returns the start position of [t]. *)
    val start : t -> position

    (** [stop t] returns the stop (or end) position of [t]. *)
    val stop : t -> position

    (** [initial] is the empty range. *)
    val initial : t

    (** [merge t1 t2] returns the merged interval {e \[start, stop)} where 
        [start = min start1 start2] and [stop = max stop1 stop2]. *)
    val merge : t -> t -> t

    val inter : t -> t -> t

    (** [are_disjoint t1 t2] returns whether the two ranges [t1] and [t2] are disjoint. *)
    val are_disjoint : t -> t -> bool

    (** [contains t pos] returns whether the position [pos] is within the range [t]. *)
    val contains : t -> position -> bool
  end

  module Make (P : Position) : S with type position := P.t
  module Line_index : S with type position := Line_index.t
  module Column_index : S with type position := Column_index.t
  include S with type position := Byte_index.t

  (** [of_string str] returns the range {e \[0, String.length str)}. *)
  val of_string : string -> t

  (** [of_lex start stop] is equivalent to [create (Byte_index.of_lex start) (Byte_index.of_lex stop)]. *)
  val of_lex : Lexing.position -> Lexing.position -> t
end

(** {3 File} 
    
    Grace provides the abstraction of a file. Simply, a file consists of a file name (used for printing)
    and the file contents. 

    There are several benefits with providing a in-memory abstraction of a file:
    - "Virtual" files. It is often useful to invent temporary files (e.g. test input, command line input, REPL input).
      By providing a in-memory abstraction, Grace provides the ability to create virtual files.
    - File caching. Caching files is useful in many situations (e.g. LSP semantic analysis, reporting multiple errors in a single file, etc). 
*)

module File : sig
  module Id : Identifiable.S

  (** The abstract type of a file. *)
  type t

  type file := t

  (** [create name source] *)
  val create : string -> string -> t

  (** [name t] returns the name associated with the file [t]. *)
  val name : t -> string

  (** [source t] returns the source associated with the file [t]. *)
  val source : t -> string

  module Line : sig
    (** [start file line_idx] returns the byte index of the beginning of the line [line_idx] in [file]. *)
    val start : t -> Line_index.t -> Byte_index.t option

    (** [range file line_idx] returns the entire range of the line [line_idx] in [file].
        Equivalent to [Range.create (start file line_idx) (start file (line_idx + 1))]. *)
    val range : t -> Line_index.t -> Range.t option

    (** [index file byte_idx] returns the line containing [byte_idx] in [file]. *)
    val index : t -> Byte_index.t -> Line_index.t

    (** [last file] returns the last line index in [file]. *)
    val last : t -> Line_index.t

    (** [slice file line_idx] returns the line source associated with [line_idx]. *)
    val slice : t -> Line_index.t -> string option
  end

  module Source : sig
    (** [range t] returns the range of the entire file [t]. *)
    val range : t -> Range.t

    (** [slice t range] returns the source associated with the range [range], *)
    val slice : t -> Range.t -> string
  end

  module Line_range : sig
    (** A line range is a column range within a given line. *)
    type t = private
      { line : Line_index.t
      ; range : Range.Column_index.t
      }
    [@@deriving sexp]

    (** [start file range] returns the first line range in [range]. *)
    val start : file -> Range.t -> t

    (** [next file range curr] returns the next line range after [curr] in [range].
        Returns [None] if [curr] is the last line range. *)
    val next : file -> Range.t -> t -> t option

    (** [slice file t] returns the source associated with [t], *)
    val slice : file -> t -> string

    (** [all file range] returns all line ranges in [range]. *)
    val all : file -> Range.t -> t list
  end
end

(** {4 Files} 
    
    Files is simply implementation of a file cache indexed by [File.Id.t]. It's signature mimics [File]'s.
*)
module Files : sig
  open File

  type t

  val create : unit -> t
  val add : t -> string -> string -> Id.t
  val find : t -> Id.t -> File.t
  val name : t -> Id.t -> string
  val source : t -> Id.t -> string

  module Line : sig
    val start : t -> Id.t -> Line_index.t -> Byte_index.t option
    val range : t -> Id.t -> Line_index.t -> Range.t option
    val index : t -> Id.t -> Byte_index.t -> Line_index.t
    val last : t -> Id.t -> Line_index.t
    val slice : t -> Id.t -> Line_index.t -> string option
  end

  module Source : sig
    val range : t -> Id.t -> Range.t
    val slice : t -> Id.t -> Range.t -> string
  end

  module Line_range : sig
    val start : t -> Id.t -> Range.t -> Line_range.t
    val next : t -> Id.t -> Range.t -> Line_range.t -> Line_range.t option
    val slice : t -> Id.t -> Line_range.t -> string
    val all : t -> Id.t -> Range.t -> Line_range.t list
  end
end

(** {4 Diagnostics} 

    A diagnostic is a collection of messages related to a single error/warning. 
    Diagnostics are rendered by the rendering engine. 
*)

module Diagnostic : sig
  module Severity : sig
    (** Severity levels *)
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
    (** The type of a message. 

        Messages are unrendered formatted strings. The rendering is delayed till Grace's renderering engine 
        since layout decisions are it's responsibility.
    *)
    type t = Formatter.t -> unit [@@deriving sexp]

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
  end

  module Label : sig
    (** A label is message associated with some source information *)
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
