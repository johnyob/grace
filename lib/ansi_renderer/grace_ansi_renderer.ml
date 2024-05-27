open! Import
module Config = Config

let of_snippet_renderer snippet_of_diagnostic ~config ppf diagnostic =
  let snippet = Source_reader.with_reader @@ fun () -> snippet_of_diagnostic diagnostic in
  Snippet_renderer.pp_snippet ~config ppf snippet
;;

let pp_diagnostic = of_snippet_renderer Snippet.of_diagnostic
let pp_compact_diagnostic = of_snippet_renderer Snippet.compact_of_diagnostic
