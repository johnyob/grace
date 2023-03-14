open Core

module type Index = sig
  type t = private int [@@deriving hash, sexp]

  include Comparable.S with type t := t
  include Invariant.S with type t := t

  val create : int -> t
  val pp : t Fmt.t
  val initial : t
  val ( + ) : t -> int -> t
  val ( - ) : t -> int -> t
end

module Int_index = struct
  module T = struct
    type t = int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let invariant t = Invariant.invariant [%here] t sexp_of_t (fun () -> assert (t >= 0))
  let pp = Fmt.int

  let create t =
    invariant t;
    t
  ;;

  let initial = 0

  let ( + ) t off =
    let t = t + off in
    invariant t;
    t
  ;;

  let ( - ) t off =
    let t = t - off in
    invariant t;
    t
  ;;
end

module Line_index = Int_index
module Column_index = Int_index
module Byte_index = Int_index

module type Number = sig
  type index
  type t [@@deriving sexp]

  include Comparable.S with type t := t

  val pp : t Fmt.t
  val of_index : index -> t
end

module Int_number = struct
  include Int

  let of_index t = t + 1
end

module Line_number = Int_number
module Column_number = Int_number
