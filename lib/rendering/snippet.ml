open Core
open Grace_core
open Text
open Diagnostic

(* Abstractions:
    - No utf8 handling (this should be done in previous step)
    - No access to files (all information for rendering should be in snippet)
*)

let styled style doc = List.fold_right ~init:doc ~f:Fmt_doc.styled style
let with_style style doc = styled style doc
let pad str ~width = Fmt_doc.(repeat (width - String.length str) sp ++ string str)

module Ctx = struct
  type t =
    { config : Config.t
    ; max_line_width : int
    ; severity : Severity.t
    }

  let ppd_outer_gutter { max_line_width; _ } = Fmt_doc.(repeat max_line_width sp)

  let ppd_line_number { max_line_width; config; _ } line =
    with_style config.style.line_number
    @@ pad (Line_number.to_string line) ~width:max_line_width
  ;;

  let ppd_source_border_left { config; _ } =
    Fmt_doc.(
      with_style config.style.source_border @@ string config.chars.source_border_left)
  ;;

  let ppd_caret { config; severity; _ } (priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.single_primary_caret
      | Secondary -> config.chars.single_secondary_caret
    in
    Fmt_doc.(with_style (Config.Style.label config.style priority severity) @@ string str)
  ;;

  let gutter_sep = Fmt_doc.sp

  let ppd_label { config; severity; _ } priority (label : Label.t) =
    with_style (Config.Style.label config.style priority severity)
    @@ Message.ppd label.message
  ;;

  let ppd_pointer_left { config; severity; _ } priority =
    Fmt_doc.(
      with_style (Config.Style.label config.style priority severity)
      @@ string config.chars.pointer_left)
  ;;
end

module Gutter = struct
  module T = struct
    type t =
      [ `Top
      | `Verticle
      | `Bottom
      ]
    [@@deriving equal, compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let ppd ~(ctx : Ctx.t) ~priority t =
    let str =
      match t with
      | `Top -> ctx.config.chars.multi_top
      | `Verticle -> ctx.config.chars.multi_left
      | `Bottom -> ctx.config.chars.multi_bottom_left
    in
    Fmt_doc.(
      with_style (Config.Style.label ctx.config.style priority ctx.severity) @@ string str)
  ;;
end

module Locus = struct
  type t =
    { file_name : string
    ; loc : Location.t
    }
  [@@deriving sexp]

  let ppd { file_name; loc } =
    Fmt_doc.(
      concat
        ~sep:(char ':')
        [ string file_name; Line_number.ppd loc.line; Column_number.ppd loc.column ])
  ;;
end

module Single_label = struct
  module Gutters = struct
    type t = { vert_gutters : Priority.t list } [@@deriving sexp]

    let ppd ~ctx { vert_gutters } =
      Fmt_doc.(
        concat
          ~sep:Ctx.gutter_sep
          (List.map vert_gutters ~f:(fun priority -> Gutter.ppd ~ctx ~priority `Verticle)))
    ;;
  end

  type t =
    { gutters : Gutters.t
    ; column_span : Span.Column_number.t
    ; carets : Priority.t option list
    ; trailing_label : label option
    ; hanging_labels : label list
    }
  [@@deriving sexp]

  and label = Priority.t * Span.Column_number.t * Label.t

  let ppd ~ctx { gutters; column_span; carets; trailing_label; hanging_labels } =
    let module Column_span = Span.Column_number in
    let prefix =
      Fmt_doc.(
        Ctx.ppd_outer_gutter ctx
        ++ sp
        ++ Ctx.ppd_source_border_left ctx
        ++ sp
        ++ Gutters.ppd ~ctx gutters
        ++ sp)
    in
    let trailing_label =
      Fmt_doc.(
        option
          (fun (priority, _, label) -> Ctx.ppd_label ctx priority label)
          trailing_label)
    in
    let carets =
      Fmt_doc.(
        concat ~sep:empty
        @@ List.map carets ~f:(function
               | None -> sp
               | Some priority -> Ctx.ppd_caret ctx priority))
    in
    let caret_pointers ~(stop : Column_number.t) =
      (* go through column numbers up until [stop] *)
      List.range (Column_span.start column_span) stop
      |> List.map ~f:(fun col ->
             let priority =
               List.filter_map hanging_labels ~f:(fun (priority, span, _label) ->
                   Option.some_if Column_span.(contains span col) priority)
               |> List.max_elt ~compare:Priority.compare
             in
             Fmt_doc.(
               match priority with
               | Some priority -> Ctx.ppd_pointer_left ctx priority
               | None -> sp))
      |> Fmt_doc.concat
    in
    let hangling_labels =
      hanging_labels
      |> List.sort ~compare:(fun (_, span1, _) (_, span2, _) ->
             Column_span.descending span1 span2)
      |> List.map ~f:(fun (priority, span, label) ->
             Fmt_doc.(
               prefix
               ++ caret_pointers ~stop:(Column_span.start span)
               ++ Ctx.ppd_label ctx priority label))
      |> Fmt_doc.(concat ~sep:newline)
    in
    Fmt_doc.(
      prefix
      ++ carets
      ++ trailing_label
      ++ newline
      ++ caret_pointers ~stop:(Column_span.stop column_span + 1)
      ++ newline
      ++ hangling_labels)
  ;;
end

(* 

module Multi_label = struct
  module Gutters = struct
    type t = Gutter.t Option_array.t [@@deriving sexp]

    let invariant t =
      (* This invariant is tricker than initially thought
          1. Only 1 top or bottom per gutter
          2. Line level invariants:
              - If break: no top or bottom (just Verticle)
              - If Source_line: only top or bottom on rhs
              - If Single_label: no top or bottom (just verticle)
              - If Multi_label: unique corresponding gutter
      *)
      Invariant.invariant [%here] t sexp_of_t (fun () ->
          assert (
            Option_array.count
              t
              ~f:(Option.value_map ~default:false ~f:Gutter.(fun gut -> gut <> Verticle))
            <= 1))
    ;;
  end

  type t =
    { gutters : Gutters.t
    ; gutter : int
    ; kind : kind
    }

  and kind =
    | Top of Column_index.t
    | Bottom of Column_index.t * Label.t
  [@@deriving sexp]
end

module Source_line = struct
  module Gutters = struct
    type t =
      { rightmost_gutter : [ `Top | `Bottom ] option
      ; vert_gutters : int
      }
    [@@deriving sexp]
  end

  type t =
    { line : Line_number.t
    ; gutters : Gutters.t
    ; source : string
    ; ranges : (Range.t * Priority.t) list
    }
  [@@deriving sexp]
end

module Break = struct
  module Gutters = struct
    type t = { vert_gutters : int } [@@deriving sexp]
  end

  type t = { gutters : Gutters.t } [@@deriving sexp]
end

module Snippet_line = struct
  type t =
    | Source of Source_line.t
    | Break of Break.t
    | Multi_label of Multi_label.t
    | Single_label of Single_label.t
  [@@deriving sexp]
end

type t =
  { severity : Severity.t
  ; message : Message.t
  ; locus : Locus.t
  ; lines : Snippet_line.t list
  ; notes : Message.t list
  }
[@@deriving sexp] *)
