open Grace_std
open Index

module T = struct
  type t =
    { start : Byte_index.t
    ; stop : Byte_index.t
    ; source : Source.t
    }
  [@@deriving sexp]

  let compare t1 t2 =
    assert (Source.(t1.source = t2.source));
    let start_cmp = Byte_index.compare t1.start t2.start in
    if start_cmp = 0 then Byte_index.compare t1.stop t2.stop else start_cmp
  ;;
end

include T
include Comparable.Make (T)

let check_invariants { start; stop; source } =
  (* 0 <= start <= stop <= eof *)
  if Byte_index.(start > stop)
  then
    invalid_argf
      "range start %a is greater than range stop %a"
      Byte_index.pp
      start
      Byte_index.pp
      stop
      ();
  let eos = Source.length source in
  if Byte_index.(stop > of_int eos)
  then
    invalid_argf
      "range beyond end of source; stop = %a > %d = eos"
      Byte_index.pp
      stop
      eos
      ()
;;

let pp ppf { start; stop; source = _ } =
  Fmt.pf ppf "[%a, %a)" Byte_index.pp start Byte_index.pp stop
;;

let create ~source start stop =
  let t = { start; stop; source } in
  check_invariants t;
  t
;;

let initial source = create ~source Byte_index.initial Byte_index.initial

let eos source =
  let eos_index = Byte_index.of_int @@ Source.length source in
  create ~source eos_index eos_index
;;

let[@inline] source t = t.source
let[@inline] start t = t.start
let[@inline] stop t = t.stop
let[@inline] split t = t.start, t.stop

let merge t1 t2 =
  assert (Source.equal t1.source t2.source);
  let start = Byte_index.min t1.start t2.start in
  let stop = Byte_index.max t1.stop t2.stop in
  { start; stop; source = t1.source }
;;

let inter t1 t2 =
  assert (Source.equal t1.source t2.source);
  let start = Byte_index.max t1.start t2.start in
  let stop = Byte_index.min t1.stop t2.stop in
  { start; stop; source = t1.source }
;;

let are_disjoint t1 t2 =
  assert (Source.equal t1.source t2.source);
  let first, last = if Byte_index.(t1.stop < t2.stop) then t1, t2 else t2, t1 in
  Byte_index.(first.stop <= last.start)
;;

let contains { start; stop; _ } elem = Byte_index.(start <= elem && elem < stop)

let of_lex ?source (start, stop) =
  let source = Option.value source ~default:(`File start.Lexing.pos_fname) in
  create ~source (Byte_index.of_lex start) (Byte_index.of_lex stop)
;;

let of_lexbuf ?source lexbuf =
  let source = Option.value source ~default:(`File lexbuf.Lexing.lex_curr_p.pos_fname) in
  create
    ~source
    (Byte_index.of_int @@ Lexing.lexeme_start lexbuf)
    (Byte_index.of_int @@ Lexing.lexeme_end lexbuf)
;;
