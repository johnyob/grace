open! Import
module Config = Config

let default_code_to_string _code = "E????"

let of_snippet_renderer
  snippet_of_diagnostic
  ?(config = Config.default)
  ?(code_to_string = default_code_to_string)
  ()
  ppf
  diagnostic
  =
  let snippet = Source_reader.with_reader @@ fun () -> snippet_of_diagnostic diagnostic in
  Snippet_renderer.pp_snippet ~config ~code_to_string ppf snippet
;;

let pp_diagnostic ?config = of_snippet_renderer Snippet.of_diagnostic ?config

let pp_compact_diagnostic ?config =
  of_snippet_renderer Snippet.compact_of_diagnostic ?config
;;
