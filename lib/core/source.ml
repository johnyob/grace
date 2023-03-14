open Core

module type Index = sig
  type t = private int [@@deriving hash, sexp]

  include Comparable.S with type t := t
  include Invariant.S with type t := t

  val create : int -> t
  val pp : t Fmt.t
  val ppd : t -> Fmt_doc.t
  val initial : t
  val ( + ) : t -> int -> t
  val ( - ) : t -> int -> t
  val ( -. ) : t -> t -> int
end

module Int_index = struct
  module T = struct
    type t = int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let invariant t = Invariant.invariant [%here] t sexp_of_t (fun () -> assert (t >= 0))
  let pp = Fmt.int
  let ppd = Fmt_doc.int

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

  let ( -. ) t1 t2 = t1 - t2
end

module Line_index = Int_index
module Column_index = Int_index

module Byte_index = struct
  include Int_index

  let of_lex Lexing.{ pos_cnum; _ } = create pos_cnum
end

module Range = struct
  module type Position = sig
    type t [@@deriving sexp, compare]

    val pp : t Fmt.t
    val initial : t
  end

  module type S = sig
    type position
    type t [@@deriving sexp]

    include Comparable.S with type t := t
    include Invariant.S with type t := t

    val pp : t Fmt.t
    val ppd : t -> Fmt_doc.t
    val create : position -> position -> t
    val start : t -> position
    val stop : t -> position
    val initial : t
    val merge : t -> t -> t
    val inter : t -> t -> t
    val are_disjoint : t -> t -> bool
    val contains : t -> position -> bool
  end

  module Make (P : Position) = struct
    module T = struct
      type t =
        { start : P.t
        ; stop : P.t
        }
      [@@deriving sexp]

      let compare t1 t2 =
        let start_cmp = P.compare t1.start t2.start in
        if start_cmp = 0 then P.compare t1.stop t2.stop else start_cmp
      ;;
    end

    include T

    let invariant ({ start; stop } as t) =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
          assert (P.compare start P.initial >= 0);
          assert (P.compare stop start >= 0))
    ;;

    let pp ppf { start; stop } = Fmt.pf ppf "[%a, %a)" P.pp start P.pp stop
    let ppd = Fmt_doc.of_pp pp

    let create start stop =
      let t = { start; stop } in
      invariant t;
      t
    ;;

    let start t = t.start
    let stop t = t.stop
    let initial = create P.initial P.initial

    let merge t1 t2 =
      let start = Comparable.min P.compare t1.start t2.start in
      let stop = Comparable.max P.compare t1.stop t2.stop in
      { start; stop }
    ;;

    let inter t1 t2 =
      let start = Comparable.max P.compare t1.start t2.start in
      let stop = Comparable.min P.compare t1.stop t2.stop in
      { start; stop }
    ;;

    let are_disjoint t1 t2 =
      let first, last = if P.compare t1.stop t2.stop < 0 then t1, t2 else t2, t1 in
      P.compare first.stop last.start <= 0
    ;;

    let contains { start; stop } elem =
      P.compare start elem <= 0 && P.compare elem stop < 0
    ;;

    include Comparable.Make (T)
  end

  module Line_index = Make (Line_index)
  module Column_index = Make (Column_index)
  include Make (Byte_index)

  let of_string str = create Byte_index.initial (String.length str)
  let of_lex start stop = create (Byte_index.of_lex start) (Byte_index.of_lex stop)
end
