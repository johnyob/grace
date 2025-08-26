open! Import

module type Index = sig
  type t = private int [@@deriving sexp]

  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t

  val to_string : t -> string
  val of_int : int -> t
  val initial : t
  val add : t -> int -> t
  val sub : t -> int -> t
  val diff : t -> t -> int
end

module Make () = struct
  module T = struct
    type t = int [@@deriving sexp]

    let compare = Int.compare
  end

  include T
  include Comparable.Make (T)

  let pp = Fmt.int
  let to_string = Int.to_string

  let of_int t =
    assert (t >= 0);
    t
  ;;

  let initial = 0

  let add t off =
    let t = t + off in
    assert (t >= 0);
    t
  ;;

  let sub t off =
    let t = t - off in
    assert (t >= 0);
    t
  ;;

  let diff t1 t2 = t1 - t2
end

module Line_index = Make ()
module Column_index = Make ()

module Byte_index = struct
  include Make ()

  let of_lex Lexing.{ pos_cnum; _ } = of_int pos_cnum
end
