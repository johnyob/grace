open Core
open Grace
open Grace_text
open Diagnostic

module type S = sig
  type t

  val render_line
    :  t
    -> Snippet.Line.t
    -> severity:Severity.t
    -> line_num_width:int
    -> marks_width:int
    -> Fmt_doc.t
end

module Default : sig
  type t = { config : Config.t } [@@unboxed]

  include S with type t := t
end = struct
  open Snippet

  type t = { config : Config.t } [@@unboxed]

  let with_style style doc = List.fold_right ~init:doc ~f:Fmt_doc.styled style
  let pad str ~width = Fmt_doc.(repeat (width - String.length str) sp ++ string str)

  let line_number { config } ~line_num_width line =
    with_style config.style.line_number
    @@ pad (Line_number.to_string line) ~width:line_num_width
  ;;

  let source_border_left { config } =
    Fmt_doc.(
      with_style config.style.source_border @@ string config.chars.source_border_left)
  ;;

  let source_border_left_break { config } =
    Fmt_doc.(
      with_style config.style.source_border
      @@ string config.chars.source_border_left_break)
  ;;

  let caret { config } ~severity ~(priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.single_primary_caret
      | Secondary -> config.chars.single_secondary_caret
    in
    Fmt_doc.(with_style (Config.Style.label config.style priority severity) @@ string str)
  ;;

  let pointer_left { config } ~severity ~priority =
    Fmt_doc.(
      with_style (Config.Style.label config.style priority severity)
      @@ string config.chars.pointer_left)
  ;;

  let multi_top { config } ~severity ~priority =
    Fmt_doc.(
      with_style (Config.Style.label config.style priority severity)
      @@ string config.chars.multi_top)
  ;;

  let multi_bottom { config } ~severity ~priority =
    Fmt_doc.(
      with_style (Config.Style.label config.style priority severity)
      @@ string config.chars.multi_bottom)
  ;;

  let multi_caret_start { config } ~severity ~(priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.multi_primary_caret_start
      | Secondary -> config.chars.multi_secondary_caret_start
    in
    Fmt_doc.(with_style (Config.Style.label config.style priority severity) @@ string str)
  ;;

  let multi_caret_end { config } ~severity ~(priority : Priority.t) =
    let str =
      match priority with
      | Primary -> config.chars.multi_primary_caret_end
      | Secondary -> config.chars.multi_secondary_caret_end
    in
    Fmt_doc.(with_style (Config.Style.label config.style priority severity) @@ string str)
  ;;

  let snippet_start { config } =
    Fmt_doc.(with_style config.style.source_border @@ string config.chars.snippet_start)
  ;;

  let note_bullet { config } =
    Fmt_doc.(with_style config.style.note_bullet @@ string config.chars.note_bullet)
  ;;

  let render_severity { config } ~severity =
    with_style (Config.Style.header config.style severity) @@ Severity.ppd severity
  ;;

  let render_label { config } ~severity (label : Label.t) =
    with_style (Config.Style.label config.style label.priority severity)
    @@ Message.ppd label.message
  ;;

  let render_mark_kind { config } ~severity ~priority mark_kind =
    let str =
      match mark_kind with
      | `Top -> config.chars.multi_top_left
      | `Vertical -> config.chars.multi_left
      | `Bottom -> config.chars.multi_bottom_left
    in
    Fmt_doc.(with_style (Config.Style.label config.style priority severity) @@ string str)
  ;;

  let mark_sp = Fmt_doc.sp

  let render_underline t ~severity ~priority mark_kind =
    match mark_kind with
    | `Top -> multi_top t ~severity ~priority
    | `Bottom -> multi_bottom t ~severity ~priority
    | `Vertical -> assert false
  ;;

  let render_default_marks t ~marks_width ~severity (marks : Mark.t list) =
    List.range 0 marks_width
    |> List.folding_map ~init:marks ~f:(fun marks idx ->
           match marks with
           | [] ->
             (* TODO: Why do we need a trailing space here? *)
             marks, mark_sp
           | { idx = idx'; _ } :: _ when idx < idx' -> marks, mark_sp
           | { idx = idx'; _ } :: _ when idx > idx' ->
             (* broken invariant: sorted [marks] *)
             assert false
           | { kind; priority; _ } :: marks ->
             marks, render_mark_kind t ~severity ~priority kind)
    |> Fmt_doc.concat ~sep:mark_sp
  ;;

  let render_multi_label_marks t ~marks_width ~severity ~mark_idx (marks : Mark.t list) =
    List.range 0 marks_width
    |> List.folding_map ~init:(marks, mark_sp) ~f:(fun (marks, sep) idx ->
           match marks with
           | [] ->
             (* prints a trailing sep *)
             (marks, sep), sep
           | { idx = idx'; _ } :: _ when idx < idx' ->
             ( (marks, sep)
             , (* print a sep for the missing mark + a trailing sep *)
               Fmt_doc.(sep ++ sep) )
           | { idx = idx'; _ } :: _ when idx > idx' ->
             (* broken invariant: sorted [marks] *)
             assert false
           | { kind; priority; _ } :: marks ->
             (* [idx = idx'] *)
             let sep =
               if idx = mark_idx then render_underline t ~severity ~priority kind else sep
             in
             (marks, sep), Fmt_doc.(render_mark_kind t ~severity ~priority kind ++ sep))
    |> Fmt_doc.(concat ~sep:empty)
  ;;

  let render_marks t ~marks_width ~severity marks (line : Source_line.t) =
    match line with
    | Multi_label { mark_idx; _ } ->
      render_multi_label_marks t ~marks_width ~severity ~mark_idx marks
    | _ -> Fmt_doc.(render_default_marks t ~marks_width ~severity marks ++ sp)
  ;;

  let render_carets t ~severity carets =
    (* Approach:
        1. Start at col 1
        2. Iterate through carets incrementing up to the last caret *)
    List.folding_map
      carets
      ~init:Column_number.initial
      ~f:(fun col (caret_col, caret_priority) ->
        if Column_number.(col <= caret_col)
        then (
          let width = caret_col - col in
          ( caret_col + 1
          , Fmt_doc.(repeat width sp ++ caret t ~severity ~priority:caret_priority) ))
        else
          (* Only possible if two carets in the same position. Not possible *)
          assert false)
    |> Fmt_doc.(concat ~sep:empty)
  ;;

  let render_single_label t ~severity ({ carets; trailing_label } : Single_label.t) =
    let carets = render_carets t ~severity carets in
    let trailing_label =
      Fmt_doc.(option (fun label -> sp ++ render_label t ~severity label) trailing_label)
    in
    Fmt_doc.(carets ++ trailing_label)
  ;;

  let render_caret_pointers t ~severity caret_pointers =
    (* Approach:
        1. Start at col 0. 
        2. Iterate through the carets incrementing up to (but not including) 
           the current start
        3. Print pointer
        4. Continue 
    *)
    let module Column_span = Span.Column_number in
    let caret_pointers_end, caret_pointers =
      List.fold_map
        caret_pointers
        ~init:Column_number.initial
        ~f:(fun col (span, priority) ->
          let start = Column_span.start span in
          if Column_number.(col <= start)
          then (
            (* Add [start - col] spaces + pointer *)
            let width = start - col in
            start + 1, Fmt_doc.(repeat width sp ++ pointer_left t ~severity ~priority))
          else
            (* [if Column_number.(col > start)]*)
            (* Skip (only occurs if we have two labels starting at same position), return empty *)
            col, Fmt_doc.empty)
    in
    caret_pointers_end, Fmt_doc.(concat caret_pointers ~sep:empty)
  ;;

  let render_hanging_label
      t
      ~severity
      ({ pointers; label_start; label } : Hanging_label.t)
    =
    let caret_pointers_end, caret_pointers = render_caret_pointers t ~severity pointers in
    Fmt_doc.(
      caret_pointers
      ++ repeat (label_start - caret_pointers_end) sp
      ++ render_label t ~severity label)
  ;;

  let render_multi_label t ~severity ({ kind; priority; _ } : Multi_label.t) =
    let underline_kind, underline_length =
      match kind with
      | `Top start -> `Top, start
      | `Bottom (stop, _) -> `Bottom, stop
    in
    let underlines =
      Fmt_doc.(
        (* [-1] to account for start/end caret *)
        repeat (underline_length - 1)
        @@ render_underline t ~severity ~priority underline_kind)
    in
    let caret =
      match kind with
      | `Top _ -> multi_caret_start t ~severity ~priority
      | `Bottom (_, label) ->
        Fmt_doc.(
          multi_caret_end t ~severity ~priority ++ sp ++ render_label t ~severity label)
    in
    Fmt_doc.(underlines ++ caret)
  ;;

  let render_content { config } ~severity ({ source; _ } : Content.t) =
    source
    |> List.map ~f:(fun (source_slice, priority) ->
           let is_primary =
             Option.value_map ~default:false ~f:Priority.is_primary priority
           in
           Fmt_doc.(
             if is_primary
             then
               with_style (Config.Style.label config.style Primary severity)
               @@ string source_slice
             else string source_slice))
    |> Fmt_doc.(concat ~sep:empty)
  ;;

  let render_source_line t ~severity (line : Source_line.t) =
    match line with
    | Content content -> render_content t ~severity content
    | Single_label single_label -> render_single_label t ~severity single_label
    | Multi_label multi_label -> render_multi_label t ~severity multi_label
    | Caret_pointers caret_pointers ->
      let _caret_pointers_end, caret_pointers =
        render_caret_pointers t ~severity caret_pointers
      in
      caret_pointers
    | Hanging_label hanging_label -> render_hanging_label t ~severity hanging_label
    | Break -> Fmt_doc.empty
  ;;

  let render_outer_gutter ~line_num_width = Fmt_doc.(repeat line_num_width sp)

  let render_line_gutter t ~line_num_width (line : Source_line.t) =
    match line with
    | Content { line; _ } -> line_number t ~line_num_width line
    | _ -> render_outer_gutter ~line_num_width
  ;;

  let render_source_border_left t (line : Source_line.t) =
    match line with
    | Break -> source_border_left_break t
    | _ -> source_border_left t
  ;;

  let render_locus ({ file_name; position } : Locus.t) =
    Fmt_doc.(
      concat
        ~sep:(char ':')
        [ string file_name
        ; Line_number.ppd position.line
        ; Column_number.ppd position.column
        ])
  ;;

  let render_title ({ config } as t) ~severity locus title =
    let title = with_style config.style.header_message @@ Message.ppd title in
    Fmt_doc.(
      option render_locus locus ++ render_severity t ~severity ++ string ": " ++ title)
  ;;

  let render_locus t ~line_num_width locus =
    Fmt_doc.(
      render_outer_gutter ~line_num_width
      ++ sp
      ++ snippet_start t
      ++ sp
      ++ render_locus locus)
  ;;

  let render_notes t ~line_num_width (notes : Message.t list) =
    List.map notes ~f:(fun message ->
        Fmt_doc.(
          render_outer_gutter ~line_num_width
          ++ sp
          ++ note_bullet t
          ++ sp
          (* TODO: Support multiline notes *)
          (* [+ 3] due to the two spaces + 1 character for [note_bullet] *)
          ++ Message.ppd message))
    |> Fmt_doc.(concat ~sep:newline)
  ;;

  let render_raw_line t ~severity ~line_num_width (raw_line : Raw_line.t) =
    match raw_line with
    | Locus locus -> render_locus t ~line_num_width locus
    | Title { locus; title } -> render_title t ~severity locus title
    | Notes notes -> render_notes t ~line_num_width notes
    | Empty -> Fmt_doc.empty
  ;;

  let style_renderer { config } = if config.color then `Ansi_tty else `None

  let render_line t (line : Line.t) ~severity ~line_num_width ~marks_width =
    Fmt_doc.set_style_renderer (style_renderer t)
    @@
    match line with
    | Source { marks; line } ->
      Fmt_doc.(
        render_line_gutter t ~line_num_width line
        ++ sp
        ++ render_source_border_left t line
        ++ sp
        ++ render_marks t ~severity ~marks_width marks line
        ++ render_source_line t ~severity line)
    | Raw raw_line -> render_raw_line t ~severity ~line_num_width raw_line
  ;;
end

type 'a t = (module S with type t = 'a)

let line_num_width (snippet : Snippet.t) =
  Line_number.max
    (snippet.lines
    |> List.filter_map ~f:(function
           | Source { line = Content { line; _ }; _ } ->
             Some (line |> Line_number.to_string |> String.length)
           | _ -> None)
    |> List.max_elt ~compare:Line_number.compare
    |> Option.value ~default:0)
    3
;;

let marks_width (snippet : Snippet.t) =
  snippet.lines
  |> List.concat_map ~f:(function
         | Source { marks; _ } -> List.map marks ~f:(fun mark -> mark.idx)
         | _ -> [])
  |> List.max_elt ~compare:Int.compare
  |> Option.value_map ~f:(fun max_idx -> max_idx + 1) ~default:0
;;

let render (type a) (renderer : a t) (self : a) (snippet : Snippet.t) =
  let module Renderer = (val renderer) in
  let marks_width = marks_width snippet in
  let line_num_width = line_num_width snippet in
  (* Fmt.pr "Marks_width: %d\n" marks_width; *)
  Fmt_doc.(
    concat ~sep:newline
    @@ List.map snippet.lines ~f:(fun line ->
           Renderer.render_line
             self
             line
             ~line_num_width
             ~marks_width
             ~severity:snippet.severity))
;;
