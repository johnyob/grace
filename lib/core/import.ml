include Core

module Format = struct
  include Format

  let pp_of_to_string to_string ppf t = pp_print_string ppf (to_string t)
end

let invalid_argf fmt = Format.kasprintf invalid_arg fmt
let sexp_of_t_of_to_string to_string t = Sexp.Atom (to_string t)

let t_of_sexp_of_of_string of_string sexp =
  match sexp with
  | Sexp.Atom str -> of_string str
  | Sexp.List _ ->
    invalid_argf "@[<2>expected sexp atom, recieved list: @[%a@]@]" Sexp.pp_hum sexp
;;
