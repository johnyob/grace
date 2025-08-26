include Grace_std
include Ppx_compare_lib.Builtin
include Ppx_sexp_conv_lib.Conv
include Ppx_hash_lib.Std.Hash.Builtin

let sexp_of_t_of_to_string to_string t = Sexplib0.Sexp.Atom (to_string t)

let t_of_sexp_of_of_string of_string sexp =
  let open Sexplib0 in
  match sexp with
  | Sexp.Atom str -> of_string str
  | List _ ->
    invalid_argf "@[<2>expected sexp atom, recieved list: @[%a@]@]" Sexp.pp_hum sexp
;;
