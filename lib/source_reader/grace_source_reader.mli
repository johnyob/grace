open Grace_std
open Grace

(** A source reader maintains a global table mapping source descriptors to their contents
    and their {i line starts}. *)

module Source_descr : sig
  (** A source descriptor is a handle for an open {{!type:Grace.Source.t} source} *)

  type t [@@deriving sexp]

  include Comparable.S with type t := t

  (** [source sd] returns the underlying source of the descriptor. *)
  val source : t -> Source.t
end

module Line_starts : sig
  (** The type of line starts.

      For computation of line numbers from ranges, we require a function mapping {{!type:Grace.Line_index.t} line indices} to {{!type:Grace.Byte_index.t} byte indicies}
      and the maximum line index.

      A valid [line_starts] for a [source] satisfies:
      + ... *)
  type t =
    { unsafe_get : int -> Byte_index.t
    ; length : int
    }

  (** A line starts {i function} maps a given {{!type:Source_descr.t} source (descriptor)} to its line starts. *)
  type fn = Source_descr.t -> t

  val default_fn : fn
end

type error =
  [ `Already_initialized
  | `Not_initialized
  ]

exception Error of error

(** [init ()] initializes the global source reader table.

    @param line_starts_fn the line starts function used for computing line starts.
    @raise Source_reader.Error if the reader is already initialized. *)
val init : ?line_starts_fn:Line_starts.fn -> unit -> unit

(** [clear ()] clears the global source reader table. *)
val clear : unit -> unit

(** [with_reader f] runs [f] with an initialized reader table, clearing it once [f] returns.

    @param line_starts_fn the line starts function used for computing line starts.
    @raise Source_reader.Error if the reader is already initialized. *)
val with_reader : ?line_starts_fn:Line_starts.fn -> (unit -> 'a) -> 'a

(** [open_source src] opens the [source], returning its descriptor.

    @raise Source_reader.Error if the reader is not initialized. *)
val open_source : Source.t -> Source_descr.t

(** [line_starts sd] returns the (possibly cached) line starts of the source [sd].
    @raise Source_reader.Error if the reader is not initialized. *)
val line_starts : Source_descr.t -> Line_starts.t

(** [length sd] returns the length or size in bytes of [src].

    It is semantically equivalent to [Source.length src]. *)
val length : Source_descr.t -> int

(** [unsafe_get sd i] reads the ith byte of the source without performing any bounds checks on [i]. *)
val unsafe_get : Source_descr.t -> int -> char

(** [slice sd range] reads the slice of bytes defined by [range].

    @raise Invalid_argment
      if the source descriptor's underlying source is not equal to the range's source. *)
val slice : Source_descr.t -> Range.t -> string

val slicei : Source_descr.t -> Byte_index.t -> Byte_index.t -> string

module Line : sig
  (** The type of a line. *)
  type t =
    { idx : Line_index.t
    ; range : Range.t
    }
  [@@deriving sexp]

  (** [of_line_index sd idx] returns the line at index [idx] in source [sd]. *)
  val of_line_index : Source_descr.t -> Line_index.t -> t

  (** [of_byte_index sd idx] returns the line containing [idx] in source [sd]. *)
  val of_byte_index : Source_descr.t -> Byte_index.t -> t

  (** [start t] returns the {{!type:Grace.Byte_index.t} byte index} of the (inclusive) start position of the line. *)
  val start : t -> Byte_index.t

  (** [stop t] returns the {{!type:Grace.Byte_index.t} byte index} of the (exclusive) stop position of the line. *)
  val stop : t -> Byte_index.t

  (** [split t] returns the pair of {{!type:Grace.Byte_index.t} byte indices} of the line [t]. *)
  val split : t -> Byte_index.t * Byte_index.t

  (** [last sd] returns the last line in the source [sd]. *)
  val last : Source_descr.t -> t

  (** [slice t ~sd] reads the slice of bytes defined by the line's [range].

      @raise Invalid_argment
        if the source descriptor's underlying source is not equal to the line range's source. *)
  val slice : t -> sd:Source_descr.t -> string
end

(** [lines sd] returns an iterator over lines in source [sd]. *)
val lines : Source_descr.t -> Line.t Iter.t

(** [lines_in_range sd range] returns an iterator over lines in the [range] in [sd].

    @raise Invalid_argment
      if the source descriptor's underlying source is not equal to the line range's source. *)
val lines_in_range : Source_descr.t -> Range.t -> Line.t Iter.t
