open! Import
module Config = Config

let default_code_to_string _code = "E????"

let of_snippet_renderer
      snippet_of_diagnostic
      f
      ?(config = Config.default)
      ?(code_to_string = default_code_to_string)
      x
      diagnostic
  =
  let snippet = Source_reader.with_reader @@ fun () -> snippet_of_diagnostic diagnostic in
  f ~config ~code_to_string x snippet
;;

let pp_diagnostic ?config ?code_to_string ppf diagnostic =
  of_snippet_renderer
    Snippet.of_diagnostic
    Snippet_renderer.pp_snippet
    ?config
    ?code_to_string
    ppf
    diagnostic
;;

let pp_compact_diagnostic ?config ?code_to_string ppf diagnostic =
  of_snippet_renderer
    Snippet.compact_of_diagnostic
    Snippet_renderer.pp_snippet
    ?config
    ?code_to_string
    ppf
    diagnostic
;;

let output_diagnostic ?config ?code_to_string oc diagnostic =
  of_snippet_renderer
    Snippet.of_diagnostic
    Snippet_renderer.output_snippet
    ?config
    ?code_to_string
    oc
    diagnostic
;;

let output_compact_diagnostic ?config ?code_to_string oc diagnostic =
  of_snippet_renderer
    Snippet.compact_of_diagnostic
    Snippet_renderer.output_snippet
    ?config
    ?code_to_string
    oc
    diagnostic
;;

let of_snippet_renderer
      snippet_of_diagnostic
      f
      ?(config = Config.default)
      ?(code_to_string = default_code_to_string)
      diagnostic
  =
  let snippet = Source_reader.with_reader @@ fun () -> snippet_of_diagnostic diagnostic in
  f ~config ~code_to_string snippet
;;

let pr_diagnostic ?config ?code_to_string diagnostic =
  of_snippet_renderer
    Snippet.of_diagnostic
    Snippet_renderer.pr_snippet
    ?config
    ?code_to_string
    diagnostic
;;

let pr_compact_diagnostic ?config ?code_to_string diagnostic =
  of_snippet_renderer
    Snippet.compact_of_diagnostic
    Snippet_renderer.pr_snippet
    ?config
    ?code_to_string
    diagnostic
;;

let epr_diagnostic ?config ?code_to_string diagnostic =
  of_snippet_renderer
    Snippet.of_diagnostic
    Snippet_renderer.epr_snippet
    ?config
    ?code_to_string
    diagnostic
;;

let epr_compact_diagnostic ?config ?code_to_string diagnostic =
  of_snippet_renderer
    Snippet.compact_of_diagnostic
    Snippet_renderer.epr_snippet
    ?config
    ?code_to_string
    diagnostic
;;
