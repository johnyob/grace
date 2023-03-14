open Core
open Grace

(** {1 Text} 
    
    Since Grace uses byte-oritented positions and ranges, for rendering (and other purposes),
    we need translate byte-oritented positions into character positions (column numbers and line numbers). 
*)
include module type of Text

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

module Position : sig
  type t =
    { line : Line_number.t
    ; column : Column_number.t
    }
  [@@deriving sexp]

  include Comparable.S with type t := t

  val pp : t Fmt.t
  val ppd : t -> Fmt_doc.t
  val initial : t
end

module Span : sig
  module type S = Range.S

  module Line_number : sig
    include S with type position := Line_number.t

    val of_range : Range.Line_index.t -> t
  end

  module Column_number : sig
    include S with type position := Column_number.t

    val of_range : Range.Column_index.t -> line:string -> t
  end

  include S with type position := Position.t
end
