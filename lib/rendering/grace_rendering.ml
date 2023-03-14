open Core
module Config = Config

let ppd_rich ?(debug = false) ~config ~files diagnostic =
  let snippet = View.rich ~debug ~files diagnostic in
  if debug then Fmt.pr "@[Snippet: %a@]@." Sexp.pp_hum (Snippet.sexp_of_t snippet);
  Renderer.(render (module Default) { config } snippet)
;;

let ppd_compact ~config ~files diagnostic =
  let snippet = View.compact ~files diagnostic in
  Renderer.(render (module Default) { config } snippet)
;;
