open Core

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

  let of_index idx = idx + 1
end

module Column_number = struct
  include Int_number

  (* TODO: Proper unicode support *)
  let of_index idx ~line:_ = idx + 1
end

module Location = struct
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
  module Range = Source.Range

  module type S = Range.S

  module Line_number = Range.Make (Line_number)
  module Column_number = Range.Make (Column_number)
  include Range.Make (Location)
end