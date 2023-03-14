open Core
open Grace
include Text

module type Number = sig
  type t = int [@@deriving hash, sexp]

  include Invariant.S with type t := t
  include Comparable.S with type t := t

  val pp : t Fmt.t
  val ppd : t -> Fmt_doc.t
  val to_string : t -> string
  val initial : t
end

module Int_number = struct
  include Int

  let invariant t = Invariant.invariant [%here] t sexp_of_t (fun () -> assert (t >= 0))
  let ppd = Fmt_doc.of_pp pp
  let initial = 0
end

module Line_number = struct
  include Int_number

  let of_index (idx : Line_index.t) = (idx :> int) + 1
end

module Column_number = struct
  include Int_number

  (* TODO: Proper unicode support *)
  let of_index (idx : Column_index.t) ~line:_ = (idx :> int) + 1
end

module Position = struct
  module T = struct
    type t =
      { line : Line_number.t
      ; column : Column_number.t
      }
    [@@deriving equal, sexp]

    let compare t1 t2 =
      let line_cmp = Line_number.compare t1.line t2.line in
      if line_cmp = 0 then Column_number.compare t1.column t2.column else line_cmp
    ;;
  end

  include T
  include Comparable.Make (T)

  let pp ppf { line; column } =
    Fmt.pf ppf "%a:%a" Line_number.pp line Column_number.pp column
  ;;

  let ppd = Fmt_doc.of_pp pp
  let initial = { line = Line_number.initial; column = Column_number.initial }
end

module Span = struct
  module type S = Range.S

  module Line_number = struct
    include Range.Make (Line_number)

    let of_range (range : Range.Line_index.t) =
      let start, stop = Range.Line_index.(start range, stop range) in
      create (Line_number.of_index start) (Line_number.of_index stop)
    ;;
  end

  module Column_number = struct
    include Range.Make (Column_number)

    let of_range (range : Range.Column_index.t) ~line =
      let start, stop = Range.Column_index.(start range, stop range) in
      create (Column_number.of_index start ~line) (Column_number.of_index stop ~line)
    ;;
  end

  include Range.Make (Position)
end
