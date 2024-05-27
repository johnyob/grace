open! Import
module Config = Config

let pp_diagnostic ~config ppf diagnostic =
  let snippet = Source_reader.with_reader @@ fun () -> Snippet.of_diagnostic diagnostic in
  Snippet_renderer.pp_snippet ~config ppf snippet
;;
