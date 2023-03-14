open! Import

module type Index = sig
  type t = private int [@@deriving hash, sexp]

  include Comparable.S with type t := t
  include Invariant.S with type t := t
  include Pretty_printer.S with type t := t

  val to_string : t -> string
  val of_int : int -> t
  val initial : t
  val add : t -> int -> t
  val sub : t -> int -> t
  val diff : t -> t -> int
end

module Int_index = struct
  module T = struct
    type t = int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let invariant t =
    (* FIXME: Register custom exceptions for pretty printing *)
    Invariant.invariant [%here] t sexp_of_t (fun () -> assert (t >= 0))
  ;;

  let pp = Format.pp_print_int
  let to_string = Int.to_string

  let of_int t =
    invariant t;
    t
  ;;

  let initial = 0

  let add t off =
    let t = t + off in
    invariant t;
    t
  ;;

  let sub t off =
    let t = t - off in
    invariant t;
    t
  ;;

  let diff t1 t2 = t1 - t2
end

module Line_index = Int_index
module Column_index = Int_index

module Byte_index = struct
  include Int_index

  let of_lex Lexing.{ pos_cnum; _ } = of_int pos_cnum
end
