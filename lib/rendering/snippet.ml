open Core
open Grace
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

  let with_label_style { config; severity; _ } priority doc =
    with_style (Config.Style.label config.style priority severity) doc
  ;;

  let ppd_caret ({ config; _ } as ctx) (priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.single_primary_caret
      | Secondary -> config.chars.single_secondary_caret
    in
    Fmt_doc.(with_label_style ctx priority @@ string str)
  ;;

  let gutter_sep = Fmt_doc.sp

  let ppd_label ctx priority (label : Label.t) =
    with_label_style ctx priority @@ Message.ppd label.message
  ;;

  let ppd_pointer_left ({ config; _ } as ctx) priority =
    Fmt_doc.(with_label_style ctx priority @@ string config.chars.pointer_left)
  ;;

  let ppd_multi_top ctx priority =
    Fmt_doc.(with_label_style ctx priority @@ string ctx.config.chars.multi_top)
  ;;

  let ppd_multi_bottom ctx priority =
    Fmt_doc.(with_label_style ctx priority @@ string ctx.config.chars.multi_bottom)
  ;;

  let ppd_multi_caret_start ({ config; _ } as ctx) (priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.multi_primary_caret_start
      | Secondary -> config.chars.multi_secondary_caret_start
    in
    Fmt_doc.(with_label_style ctx priority @@ string str)
  ;;

  let ppd_multi_caret_end ({ config; _ } as ctx) (priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.multi_primary_caret_end
      | Secondary -> config.chars.multi_secondary_caret_end
    in
    Fmt_doc.(with_label_style ctx priority @@ string str)
  ;;

  let ppd_snippet_start { config; _ } =
    Fmt_doc.(with_style config.style.source_border @@ string config.chars.snippet_start)
  ;;

  let ppd_severity { config; severity; _ } =
    with_style (Config.Style.header config.style severity) @@ Severity.ppd severity
  ;;

  let ppd_header_message { config; _ } message =
    with_style config.style.header_message @@ Message.ppd message
  ;;

  let ppd_note_bullet { config; _ } =
    Fmt_doc.(with_style config.style.note_bullet @@ string config.chars.note_bullet)
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

module Vert_gutters = struct
  type t = Priority.t option list [@@deriving sexp]

  let ppd ~ctx vert_gutters =
    Fmt_doc.(
      concat
        ~sep:Ctx.gutter_sep
        (List.map
           vert_gutters
           ~f:(option (fun priority -> Gutter.ppd ~ctx ~priority `Verticle))))
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
  module Gutters = Vert_gutters

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
      (* Alistair: This is a semantically correct implementation of caret pointers, but is 
         rather slow (each time filtering through [hanging_labels]). Could be 
         optimised by doing some initial preprocessing on [hanging_labels]. *)
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

module Multi_label = struct
  module Underline = struct
    type t = [ `Top | `Bottom ] * Priority.t

    let ppd ~ctx (gutter, priority) =
      match gutter with
      | `Top -> Ctx.ppd_multi_top ctx priority
      | `Bottom -> Ctx.ppd_multi_bottom ctx priority
    ;;
  end

  module Gutters = struct
    type t = (Gutter.t * Priority.t) option list [@@deriving sexp]

    let ppd ~ctx ~underline ~gutter_idx t =
      (* Alistair: This was initially written as a single list pass, but it was 
         very message and unclear. It has now been split into several passes (slower), 
         but far more readible.
         
         TODO: Test performance (still O(n) which is probably good enough for us) *)
      let underline = Underline.(ppd ~ctx underline) in
      let gutters ~default t =
        List.map
          t
          ~f:
            Fmt_doc.(
              option ~none:default (fun (gutter, priority) ->
                  Gutter.ppd ~ctx ~priority gutter))
      in
      let before, after = List.split_n t gutter_idx in
      Fmt_doc.(
        concat (gutters ~default:sp before)
        ++ (if (* Space before gutters if (and only if) there are gutters before [gutter_idx] *)
               List.is_empty before
           then empty
           else sp)
        ++ concat ~sep:underline (gutters ~default:underline after))
    ;;
  end

  type t =
    { gutters : Gutters.t
    ; gutter_idx : int
    ; priority : Priority.t
    ; kind : kind
    }

  and kind =
    | Top of Column_number.t
    | Bottom of Column_number.t * Label.t
  [@@deriving sexp]

  let column_number_of_kind kind =
    match kind with
    | Top start -> start
    | Bottom (stop, _) -> stop
  ;;

  let ppd ~ctx { gutters; gutter_idx; priority; kind } =
    let underline =
      ( (match kind with
        | Top _ -> `Top
        | Bottom _ -> `Bottom)
      , priority )
    in
    let underlines =
      let stop = column_number_of_kind kind in
      Fmt_doc.(repeat stop @@ Underline.ppd ~ctx underline)
    in
    let caret =
      match kind with
      | Top _ -> Ctx.ppd_multi_caret_start ctx priority
      | Bottom (_, label) ->
        Fmt_doc.(
          Ctx.ppd_multi_caret_end ctx priority ++ sp ++ Ctx.ppd_label ctx priority label)
    in
    Fmt_doc.(
      Ctx.ppd_outer_gutter ctx
      ++ sp
      ++ Ctx.ppd_source_border_left ctx
      ++ sp
      ++ Gutters.ppd ~ctx ~gutter_idx ~underline gutters
      ++ underlines
      ++ caret)
  ;;
end

module Source_line = struct
  module Gutters = struct
    type t =
      { rightmost_gutter : ([ `Top | `Bottom ] * Priority.t) option
      ; vert_gutters : Vert_gutters.t
      }
    [@@deriving sexp]

    let ppd ~ctx { vert_gutters; rightmost_gutter } =
      Fmt_doc.(
        Vert_gutters.ppd ~ctx vert_gutters
        ++ option
             (fun (gutter, priority) ->
               Ctx.gutter_sep ++ Gutter.ppd ~ctx ~priority gutter)
             rightmost_gutter)
    ;;
  end

  type t =
    { line : Line_number.t
    ; gutters : Gutters.t
    ; source : (string * Priority.t option) list
    }
  [@@deriving sexp]

  let ppd ~ctx { line; gutters; source } =
    let source =
      source
      |> List.map ~f:(fun (source_slice, priority) ->
             let is_primary =
               Option.value_map ~default:false ~f:Priority.is_primary priority
             in
             Fmt_doc.(
               if is_primary
               then Ctx.with_label_style ctx Primary @@ string source_slice
               else string source_slice))
      |> Fmt_doc.(concat ~sep:empty)
    in
    Fmt_doc.(
      Ctx.ppd_line_number ctx line
      ++ sp
      ++ Ctx.ppd_source_border_left ctx
      ++ sp
      ++ Gutters.ppd ~ctx gutters
      ++ sp
      ++ source)
  ;;
end

module Break = struct
  module Gutters = Vert_gutters

  type t = { gutters : Gutters.t } [@@deriving sexp]

  let ppd ~ctx { gutters } =
    Fmt_doc.(
      Ctx.ppd_outer_gutter ctx
      ++ sp
      ++ Ctx.ppd_source_border_left ctx
      ++ sp
      ++ Gutters.ppd ~ctx gutters)
  ;;
end

module Snippet_line = struct
  type t =
    | Source_line of Source_line.t
    | Break of Break.t
    | Multi_label of Multi_label.t
    | Single_label of Single_label.t
  [@@deriving sexp]

  let ppd ~ctx line =
    match line with
    | Source_line source_line -> Source_line.ppd ~ctx source_line
    | Break break -> Break.ppd ~ctx break
    | Multi_label multi_label -> Multi_label.ppd ~ctx multi_label
    | Single_label single_label -> Single_label.ppd ~ctx single_label
  ;;
end

module File = struct
  type t =
    { locus : Locus.t
    ; lines : Snippet_line.t list
    }
  [@@deriving sexp]

  let ppd ~ctx { locus; lines } =
    let lines =
      Fmt_doc.(concat ~sep:newline @@ List.map lines ~f:(Snippet_line.ppd ~ctx))
    in
    Fmt_doc.(
      Ctx.ppd_outer_gutter ctx
      ++ Ctx.ppd_snippet_start ctx
      ++ sp
      ++ Locus.ppd locus
      ++ newline
      ++ lines)
  ;;
end

type t =
  { severity : Severity.t
  ; message : Message.t
  ; files : File.t list
  ; notes : Message.t list
  }
[@@deriving sexp]

let ppd ~config { severity; message; files; notes } =
  (* First we need to compute the context *)
  let max_line_width =
    (* Default minimum is 3 *)
    let default_min_line_width = 3 in
    max
      (files
      |> List.concat_map ~f:(fun { lines; _ } -> lines)
      |> List.filter_map ~f:(function
             | Source_line { line; _ } ->
               Some (line |> Line_number.to_string |> String.length)
             | _ -> None)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:default_min_line_width)
      default_min_line_width
  in
  let ctx = Ctx.{ config; severity; max_line_width } in
  let files = List.map files ~f:(File.ppd ~ctx) in
  let file_tail =
    (* Alistair: For now a newline will suffice *)
    Fmt_doc.newline
  in
  let notes =
    List.map notes ~f:(fun message ->
        Fmt_doc.(Ctx.ppd_note_bullet ctx ++ sp ++ vbox (Message.ppd message)))
    |> Fmt_doc.(concat ~sep:newline)
  in
  Fmt_doc.(
    Ctx.ppd_severity ctx
    ++ string ": "
    ++ Ctx.ppd_header_message ctx message
    ++ newline
    ++ concat ~sep:file_tail files
    ++ (if (* file tail for last file *) not (List.is_empty files)
       then file_tail
       else empty)
    ++ notes
    ++ newline)
;;
