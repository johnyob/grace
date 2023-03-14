open Core
open Index

(** Grace's {e ranges} are opaque {{!type:Byte_index.t} byte index} intervals of the form {e \[start, stop)}
    (not including the byte at the [stop] index) within a given {{!type:Source.t} [source]}.

    A range is said to be valid if:
    + All indices are valid.
    + [start <= stop], note that [start = stop] is permitted, in this case, this denotes an empty range.
    + [stop <= eos], where [eos] is the end-of-source position, also known as the {{!val:Source.length} source length}, of [source] *)

(** The abstract type of ranges. *)
type t [@@deriving equal, compare, hash, sexp]

include Comparable.S with type t := t
include Invariant.S with type t := t
include Pretty_printer.S with type t := t

(** [create ~source start stop] builds the range {e \[start, stop)} (not including the byte at the ending position) from the byte indices [start] and [stop].
    A range is empty if its start and stop indices are the same.

    @param source the associated source of the range.
    @raise Invalid_argument
      if the [start] is greater than [stop] or [stop] is greater than the end-of-source position in [source]. *)
val create : source:Source.t -> Byte_index.t -> Byte_index.t -> t

(** [initial source] is the initial range, namely {e \[0, 0)}. *)
val initial : Source.t -> t

(** [eos source] is the end-of-source range {e \[eos, eos)} for the given [source]. *)
val eos : Source.t -> t

(** [source t] returns the source associated with [t]. *)
val source : t -> Source.t

(** [start t] returns the {{!type:Byte_index.t} byte index} of the (inclusive) start position. *)
val start : t -> Byte_index.t

(** [stop t] returns the {{!type:Byte_index.t} byte index} of the (exclusive) stop position. *)
val stop : t -> Byte_index.t

(** [split t] returns the pair of {{!type:Byte_index.t} byte indices} of [t]. *)
val split : t -> Byte_index.t * Byte_index.t

(** [merge t1 t2] returns the merged interval {e \[start, stop)} where [start = min start1 start2] and [stop = max stop1 stop2].

    @raise Invalid_argument
      if the two ranges have differing sources. The comparison of sources is performed by [Source.equal]. *)
val merge : t -> t -> t

(** [inter t1 t2] returns the intersectional interval {e \[start, stop)} where [start = min start1 start2] and [stop = max stop1 stop2].

    @raise Invalid_argument
      if the two ranges have differing sources. The comparison of sources is performed by [Source.equal]. *)
val inter : t -> t -> t

(** [are_disjoint t1 t2] returns whether the two ranges [t1] and [t2] are disjoint. *)
val are_disjoint : t -> t -> bool

(** [contains t idx] returns whether the position [idx] is within the range [t]. *)
val contains : t -> Byte_index.t -> bool

(** {1 Support for Lexing} *)

(** [of_lex (start, stop)] takes a pair of OCaml lexer positions and creates a range. It is equivalent to
    [make ~source:default (Byte_index.of_lex start) (Byte_index.of_lex stop)].

    @param source
      The source of the new range. The default source is [`File start.pos_fname].

    @raise Invalid_argument
      if the optional argument [source] is not given and [start.pos_fname] and [stop.pos_fname] differ. The comparison is done by [String.equal] without any path canonicalization. *)
val of_lex : ?source:Source.t -> Lexing.position * Lexing.position -> t

(** [of_lexbuf lexbuf] constructs a range from the current lexeme that [lexbuf] points to. It is [of_lex (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)].

    @param source
      The source of the new range. The default source is [`File (Lexing.lexeme_start_p lexbuf).pos_fname]. *)
val of_lexbuf : ?source:Source.t -> Lexing.lexbuf -> t
