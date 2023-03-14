module Config = Config

let ppd_rich ~config ~files diagnostic =
  let snippet = View.rich ~files diagnostic in
  Renderer.(render (module Default) { config } snippet)
;;

let ppd_compact ~config ~files diagnostic =
  let snippet = View.compact ~files diagnostic in
  Renderer.(render (module Default) { config } snippet)
;;
