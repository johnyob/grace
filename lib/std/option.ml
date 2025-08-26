open Grace_std0
include Stdlib.Option

let sexp_of_t = Sexplib.Std.sexp_of_option
let t_of_sexp = Sexplib.Std.option_of_sexp

let get ?here t =
  try get t with
  | Invalid_argument msg as exn ->
    (match here with
     | None -> raise exn
     | Some here -> invalid_argf "%s: %s" here msg)
;;
