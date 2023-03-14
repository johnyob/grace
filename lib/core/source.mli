(** Grace provides the abstraction of a file called a 'source'.

    There are several benefits with providing a in-memory abstraction of a sources:
    + {b Virtual files}: It is often useful to invent temporary files (e.g. test input, command line input, REPL input). By providing a in-memory abstraction, Grace provides the ability to create virtual files.
    + {b Caching}: Caching sources is useful in many situations (e.g. LSP semantic analysis, reporting multiple errors in a single file in a diagnostic reporter, etc). *)

(** A reader denotes an arbitrary byte source (potentially backed by a file, buffer, socket, etc). *)
type reader =
  { id : int
  (** The unique identifier of the reader. Equality, comparison, hashing are all performed on this identifier. *)
  ; name : string option
  (** The name of the reader. The diagnostic render can use the name of the reader in place of a file path. *)
  ; length : int (** The length (in bytes) of the source. *)
  ; unsafe_get : int -> char
  (** [unsafe_get i] reads the [i]th byte without performing bounds checks. *)
  }
[@@deriving equal, compare, hash, sexp]

(** [reader_name reader] returns the name of the reader. If [reader.name] is [None], then identifier [reader.id] (converted to a string) is returned. *)
val reader_name : reader -> string

(** An in-memory string source. *)
type string_source =
  { name : string option
  (** The name of a string source. The diagnostic render can use the name of a string source in place of a file path. *)
  ; content : string (** The content of a string source *)
  }
[@@deriving equal, compare, hash, sexp]

(** The type of sources. *)
type t =
  [ `File of string (** A file source specified by its filename. *)
  | `String of string_source (** A in-memory string source. *)
  | `Reader of reader (** A reader-backed source. *)
  ]
[@@deriving equal, compare, hash, sexp]

(** [name src] returns the name of the source if it exists. *)
val name : t -> string option

(** [length src] returns the length or size in bytes of [src]. Interpreted as a {{!type:Index.Byte_index.t} byte_index}, this is known as the end-of-source position.

    @raise Invalid_argument if the file size is larger than an OCaml 63-bit integer. *)
val length : t -> int
