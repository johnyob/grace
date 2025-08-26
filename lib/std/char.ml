module T = Stdlib.Char
include T
include Comparable.Make (T)

let pp = Format.pp_print_char
let sexp_of_t = Sexplib.Std.char_of_sexp
let t_of_sexp = Sexplib.Std.sexp_of_char

module Ascii = struct
  let is_whitespace = function
    | '\t' | '\n' | '\001' | '\012' | '\r' | ' ' -> true
    | _ -> false
  ;;
end
