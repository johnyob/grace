open Core
open Index

module Location = struct
  type t =
    { line : Line_index.t
    ; column : Column_index.t
    }
  [@@deriving sexp]

  let pp ppf { line; column } =
    Fmt.pf ppf "%a:%a" Line_index.pp line Column_index.pp column
  ;;

  let create line column = { line; column }
end

module Range = struct
  module T = struct
    type t =
      { start : Byte_index.t
      ; stop : Byte_index.t
      }
    [@@deriving equal, sexp]

    let compare t1 t2 =
      let start_cmp = Byte_index.compare t1.start t2.start in
      if start_cmp = 0 then Byte_index.compare t1.stop t2.stop else start_cmp
    ;;
  end

  include T

  let invariant ({ start; stop } as t) =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        assert (Byte_index.compare start Byte_index.initial >= 0);
        assert (Byte_index.compare stop start >= 0))
  ;;

  let pp ppf { start; stop } =
    Fmt.pf ppf "[%a, %a)" Byte_index.pp start Byte_index.pp stop
  ;;

  let create start stop =
    let t = { start; stop } in
    invariant t;
    t
  ;;

  let initial = create Byte_index.initial Byte_index.initial

  let merge t1 t2 =
    let start = Comparable.min Byte_index.compare t1.start t2.start in
    let stop = Comparable.max Byte_index.compare t1.stop t2.stop in
    { start; stop }
  ;;

  let are_disjoint t1 t2 =
    let first, last = if Byte_index.compare t1.stop t2.stop < 0 then t1, t2 else t2, t1 in
    Byte_index.compare first.start last.start <= 0
  ;;

  let contains { start; stop } elem =
    Byte_index.compare start elem <= 0 && Byte_index.compare elem stop < 0
  ;;

  let from_string string = create Byte_index.initial (String.length string)

  include Comparable.Make (T)
end
