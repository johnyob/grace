module T = Stdlib.Int
include T
include Comparable.Make (T)

let pp = Format.pp_print_int
let sexp_of_t = Sexplib.Std.sexp_of_int
let t_of_sexp = Sexplib.Std.int_of_sexp
