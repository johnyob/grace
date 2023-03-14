open Core

(** Index types are integer-like types that specify byte-indexed positions in a source file.
    All indices are 0-indexed and positive.

    Grace uses byte-indexed positions are they are the most portable location information.
    Alternatives include:
    - Character positions
    - Column and line numbers

    Neither alternatives are well-defined without contextual information (encodings, etc). *)

module type Index = sig
  (** The type of indexes. An integer with the invariant that an index [t] satisfies [t >= 0]. *)
  type t = private int [@@deriving equal, compare, hash, sexp]

  include Comparable.S with type t := t
  include Invariant.S with type t := t
  include Pretty_printer.S with type t := t

  val to_string : t -> string

  (** [of_int n] creates the index [n].

      @raise Invalid_argument if [n < 0]. *)
  val of_int : int -> t

  (** [initial] is the initial index, namely [0]. *)
  val initial : t

  (** [add t off] adds the offset [off] to the index [t]. *)
  val add : t -> int -> t

  (** [sub t off] subtracts the offset [off] from the index [t]. *)
  val sub : t -> int -> t

  (** [diff t1 t2] returns the (potentially negative) difference between [t1] and [t2]. *)
  val diff : t -> t -> int
end

module Line_index : Index
module Column_index : Index

module Byte_index : sig
  include Index (** @inline *)

  (** {1 Support for Lexing} *)

  (** [of_lex lex_pos] returns the byte index from the lexing position [lex_pos]. It is equivalent to [create (lex_pos.pos_cnum)]. *)
  val of_lex : Lexing.position -> t
end
