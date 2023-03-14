module Config = Config
module Snippet = Snippet

let ppd_rich ~config ~files diagnostic =
  let snippet = View.rich ~files diagnostic in
  Renderer.(render (module Default) { config } snippet)
;;
