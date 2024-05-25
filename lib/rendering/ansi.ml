open! Import
open Diagnostic
open Snippet
open Config
module Multi_id = Multi_line_label.Id

let pp_label_styled ~(config : Config.t) ~severity ~priority pp =
  Fmt.styled_multi (Style_sheet.label config.styles priority severity) pp
;;

let pp_label_styled_string ~config ~severity ~priority =
  pp_label_styled ~config ~severity ~priority Fmt.string
;;

module Chars = struct
  let pp_source_border_left ~(config : Config.t) ppf () =
    Fmt.(
      styled_multi config.styles.source_border string ppf config.chars.source_border_left)
  ;;

  let pp_source_border_left_break ~(config : Config.t) ppf () =
    Fmt.(
      styled_multi
        config.styles.source_border
        string
        ppf
        config.chars.source_border_left_break)
  ;;

  let pp_caret ~(config : Config.t) ~severity ~priority ppf () =
    let caret =
      match priority with
      | Priority.Primary -> config.chars.single_primary_caret
      | Secondary -> config.chars.single_secondary_caret
    in
    pp_label_styled_string ~config ~severity ~priority ppf caret
  ;;

  let pp_pointer_left ~(config : Config.t) ~severity ~priority ppf () =
    pp_label_styled_string ~config ~severity ~priority ppf config.chars.pointer_left
  ;;

  let pp_multi_top ~(config : Config.t) ~severity ~priority ppf () =
    pp_label_styled_string ~config ~severity ~priority ppf config.chars.multi_top
  ;;

  let pp_multi_bottom ~(config : Config.t) ~severity ~priority ppf () =
    pp_label_styled_string ~config ~severity ~priority ppf config.chars.multi_bottom
  ;;

  let pp_multi_caret_start ~(config : Config.t) ~severity ~priority ppf () =
    let caret_start =
      match priority with
      | Priority.Primary -> config.chars.multi_primary_caret_start
      | Secondary -> config.chars.multi_secondary_caret_start
    in
    pp_label_styled_string ~config ~severity ~priority ppf caret_start
  ;;

  let pp_multi_caret_end ~(config : Config.t) ~severity ~priority ppf () =
    let caret_end =
      match priority with
      | Priority.Primary -> config.chars.multi_primary_caret_end
      | Secondary -> config.chars.multi_secondary_caret_end
    in
    pp_label_styled_string ~config ~severity ~priority ppf caret_end
  ;;

  let pp_snippet_start ~(config : Config.t) ppf () =
    Fmt.styled_multi config.styles.source_border Fmt.string ppf config.chars.snippet_start
  ;;

  let pp_note_bullet ~(config : Config.t) ppf () =
    Fmt.styled_multi config.styles.note_bullet Fmt.string ppf config.chars.note_bullet
  ;;
end

let pp_severity ~(config : Config.t) ppf severity =
  Fmt.with_style (Style_sheet.header config.styles severity) ppf
  @@ fun ppf () -> Severity.pp ppf severity
;;

let pp_header_message ~(config : Config.t) =
  Fmt.styled_multi config.styles.header_message Message.pp
;;

let pp_message ~(config : Config.t) ~severity ~priority ppf message =
  Fmt.styled_multi
    (Style_sheet.label config.styles priority severity)
    Message.pp
    ppf
    message
;;

type multi_kind =
  [ `Top
  | `Vertical
  | `Bottom
  ]

module Multi_context = struct
  type t =
    { gutters : (Priority.t * multi_kind) Option_array.t
    ; bindings : int Multi_id.Table.t
    }

  let create ~len =
    { gutters = Option_array.create ~len; bindings = Multi_id.Table.create () }
  ;;

  let length t = Option_array.length t.gutters

  let def t ~multi_id ~priority prologue =
    assert (not (Hashtbl.mem t.bindings multi_id));
    let gutter, _ =
      (* Find the next free available gutter (one such gutter *must* exist!) *)
      t.gutters
      |> Option_array.findi ~f:(fun _ gutter -> Option.is_none gutter)
      |> Option.value_exn ~here:[%here]
    in
    Hashtbl.set t.bindings ~key:multi_id ~data:gutter;
    (* Set gutter to `Top *)
    Option_array.set_some t.gutters gutter (priority, `Top);
    (* Execute 'prologue' for multi-line label *)
    prologue ();
    (* Set gutter to `Vertical *)
    Option_array.set_some t.gutters gutter (priority, `Vertical)
  ;;

  let free t ~multi_id epilogue =
    let gutter = Hashtbl.find_exn t.bindings multi_id in
    let priority, _ = Option_array.get_some_exn t.gutters gutter in
    (* Set gutter to `Bottom *)
    Option_array.set_some t.gutters gutter (priority, `Bottom);
    (* Execute 'epilogue' for multi-line label *)
    epilogue ();
    (* Remove bindings for multi-line label *)
    Option_array.set_none t.gutters gutter;
    Hashtbl.remove t.bindings multi_id
  ;;
end

let pp_multi_vertline ~(config : Config.t) ~severity ~priority ppf kind =
  let gutter =
    match kind with
    | `Top -> config.chars.multi_top_left
    | `Vertical -> config.chars.multi_left
    | `Bottom -> config.chars.multi_bottom_left
  in
  pp_label_styled_string ~config ~severity ~priority ppf gutter
;;

let pp_multi_underline ~(config : Config.t) ~severity ~priority ppf kind =
  match kind with
  | `Top -> Chars.pp_multi_top ~config ~severity ~priority ppf ()
  | `Bottom -> Chars.pp_multi_bottom ~config ~severity ~priority ppf ()
  | `Vertical -> Fmt.sp ppf ()
;;

let pp_multi_lines ~(config : Config.t) ~severity ppf (mctxt : Multi_context.t) =
  let set_sep, pr_sep =
    let sep = ref Fmt.sp in
    (fun sep' -> sep := sep'), fun () -> !sep ppf ()
  in
  for i = 0 to Multi_context.length mctxt - 1 do
    match Option_array.get mctxt.gutters i with
    | None ->
      (* Print the [sep] for the missing gutter and a trailing separator. *)
      pr_sep ();
      pr_sep ()
    | Some (priority, kind) ->
      (* Set the separate (if necessary) *)
      set_sep (fun ppf () -> pp_multi_underline ~config ~severity ~priority ppf kind);
      (* Print the gutter line and a trailing separator. *)
      pp_multi_vertline ~config ~severity ~priority ppf kind;
      pr_sep ()
  done;
  (* Print a trailing separator *)
  pr_sep ()
;;

type context =
  { line_num_width : int
  ; multi_context : Multi_context.t
  }

let pp_line_number ~(config : Config.t) ~ctxt ppf (lnum : Line_number.t) =
  Fmt.with_style config.styles.line_number ppf
  @@ fun ppf () -> Fmt.pf ppf "%*d" ctxt.line_num_width (lnum :> int)
;;

let pp_source_line ~config ~severity ~ctxt ~lnum ppf (line : Line.t) =
  let pp_segment ppf (segment : Line.segment) =
    let content =
      (* FIXME: We should strip the content in [Snippet.of_diagnostic] *)
      String.rstrip segment.content ~drop:(function
        | '\n' | '\r' -> true
        | _ -> false)
    in
    match segment.stag with
    | Some { priority = Primary; _ } ->
      (* If primary, style the content *)
      pp_label_styled_string ~config ~severity ~priority:Primary ppf content
    | _ ->
      (* Otherwise, simply print the content *)
      Fmt.string ppf content
  in
  Fmt.(
    pf
      ppf
      "@[<h>%a %a %a%a@]"
      (pp_line_number ~config ~ctxt)
      lnum
      (Chars.pp_source_border_left ~config)
      ()
      (pp_multi_lines ~config ~severity)
      ctxt.multi_context
      (list ~sep:nop pp_segment)
      line.segments)
;;

let pp_line_break ~config ~severity ~ctxt ppf () =
  Fmt.pf
    ppf
    "@[<h>%*s %a %a@]"
    ctxt.line_num_width
    ""
    (Chars.pp_source_border_left_break ~config)
    ()
    (pp_multi_lines ~config ~severity)
    ctxt.multi_context
;;

(* prefixed box *)
let pbox ~prefix pp ppf x =
  let s = Fmt.str_like ppf "%a" pp x in
  let lines = String.split_lines s in
  let nlines = List.length lines in
  List.iteri lines ~f:(fun i line ->
    Fmt.pf ppf "@[<h>%s%s@]" prefix line;
    if i <> nlines - 1 then Fmt.newline ppf ())
;;

(* prefixed-with-indent box *)
let pwibox ~prefix pp ppf x =
  let s = Fmt.str_like ppf "%a" pp x in
  let lines = String.split_lines s in
  let nlines = List.length lines in
  let nprefix = String.length prefix in
  List.iteri lines ~f:(fun i line ->
    if i = 0
    then Fmt.pf ppf "@[<h>%s%s@]" prefix line
    else Fmt.pf ppf "@[<h>%*s%s@]" nprefix "" line;
    if i <> nlines - 1 then Fmt.newline ppf ())
;;

(* line box *)
let lbox ~config ~severity ~ctxt pp ppf x =
  let prefix =
    Fmt.str_like
      ppf
      "%*s %a %a"
      ctxt.line_num_width
      ""
      (Chars.pp_source_border_left ~config)
      ()
      (pp_multi_lines ~config ~severity)
      ctxt.multi_context
  in
  pbox ~prefix pp ppf x
;;

module Multi_line_label = struct
  let pp_underlines ~config ~severity ~priority ~width =
    Fmt.repeat ~width (pp_multi_underline ~config ~severity ~priority)
  ;;

  let pp_top ~config ~severity ppf (width, priority) =
    pp_underlines ~config ~severity ~priority ~width ppf `Top;
    Chars.pp_multi_caret_start ~config ~severity ~priority ppf ()
  ;;

  let pp_bottom ~config ~severity ppf (width, priority, label) =
    let prefix =
      Fmt.str_like
        ppf
        "%a%a "
        (pp_underlines ~config ~severity ~priority ~width)
        `Bottom
        (Chars.pp_multi_caret_end ~config ~severity ~priority)
        ()
    in
    pwibox ~prefix (pp_message ~config ~severity ~priority) ppf label
  ;;

  let pp_content_top ~ctxt ~(top : Multi_line_label.t option) pp ppf x =
    match top with
    | Some mll ->
      (match mll with
       | Top { id = multi_id; priority; _ } ->
         Multi_context.def ctxt.multi_context ~multi_id ~priority @@ fun () -> pp ppf x
       | _ -> assert false)
    | None -> pp ppf x
  ;;

  let pp ~config ~severity ~ctxt ppf (multi_line_label : Multi_line_label.t) =
    match multi_line_label with
    | Bottom { id; stop; priority; label } ->
      Multi_context.free ctxt.multi_context ~multi_id:id
      @@ fun () ->
      pp_multi_lines ~config ~severity ppf ctxt.multi_context;
      pp_bottom
        ~config
        ~severity
        ppf
        (* [-2] since we want a [-1] offset and [stop] is a column number (starting at 1) *)
        ((stop :> int) - 2, priority, label)
    | Top { id; start; priority } ->
      Multi_context.def ctxt.multi_context ~multi_id:id ~priority
      @@ fun () ->
      pp_multi_lines ~config ~severity ppf ctxt.multi_context;
      (* [-1] since [start] is a column number (starting at 1) *)
      pp_top ~config ~severity ppf ((start :> int) - 1, priority)
  ;;
end

module Inline_labels = struct
  (** An inline segment with dangling pointers, with optional messages. *)
  type hanging_segment =
    { offset : Column_number.t (** The offset into the line *)
    ; length : int (** The length of the segment > 0 *)
    ; priority : Priority.t
    ; messages : Message.t list
    }

  type trailing_segment =
    { offset : Column_number.t
    ; length : int
    ; priority : Priority.t
    ; message : Message.t
    }

  (** A rendering IR for inline labels *)
  type t =
    { trailing_segment : trailing_segment option (** An optional trailing label *)
    ; hanging_segments : hanging_segment list
    (** A lexically sorted list of hanging segments *)
    }

  let is_empty { hanging_segments; trailing_segment } =
    List.is_empty hanging_segments && Option.is_none trailing_segment
  ;;

  let pp_trailing_label ~config ~severity =
    Fmt.(
      option ~none:nop
      @@ fun ppf ({ message; priority; _ } : trailing_segment) ->
      Fmt.pf ppf " %a" (pp_message ~config ~severity ~priority) message)
  ;;

  let pp_carets ~config ~severity ppf { hanging_segments; trailing_segment } =
    (* [cursor] is used to keep track of the position in the current line buffer *)
    let cursor = ref Column_number.initial in
    let pr_segment (offset, length, priority) =
      assert (Column_number.(!cursor <= offset));
      (* Print spaces up until [range] *)
      Fmt.sps Column_number.(diff offset !cursor) ppf ();
      (* Print carets *)
      Fmt.(repeat ~width:length @@ Chars.pp_caret ~config ~severity ~priority) ppf ();
      (* Update cursor to be stop *)
      cursor := Column_number.(add offset length)
    in
    (* Render the carets for hanging and trailing segments *)
    List.iter hanging_segments ~f:(fun { offset; length; priority; _ } ->
      pr_segment (offset, length, priority));
    Option.iter trailing_segment ~f:(fun { offset; length; priority; _ } ->
      pr_segment (offset, length, priority))
  ;;

  let pp_hanging_segments ~config ~severity ppf segments =
    let str_pointer_left priority =
      Fmt.str_like ppf "%a" (Chars.pp_pointer_left ~config ~severity ~priority) ()
    in
    let pp_messages ~priority =
      let open Fmt in
      vbox @@ list ~sep:newline @@ hbox @@ pp_message ~config ~severity ~priority
    in
    let rec loop cursor pointers = function
      | [] ->
        (* Print the initial hanging pointers *)
        Fmt.pf ppf "%s" pointers
      | { offset; length; priority = _; messages = [] } :: segments ->
        assert (Column_number.(cursor <= offset));
        (* In the case of an empty hanging segment, simply print the spaces and move the cursor *)
        let pointers =
          String.append
            pointers
            (String.make Column_number.(diff (add offset length) cursor) ' ')
        in
        loop Column_number.(add offset length) pointers segments
      | { offset; length = _; priority; messages = _ :: _ as messages } :: segments ->
        assert (Column_number.(cursor <= offset));
        (* Prefix the pointers with spaces *)
        let pointers =
          String.append pointers (String.make Column_number.(diff offset cursor) ' ')
        in
        (* Print the pointers & messages above this message adding a pointer for this set of messages
           Invariant: offset + length >= offset + 1 <=> length > 0 *)
        loop
          Column_number.(add offset 1)
          String.(pointers ^ str_pointer_left priority)
          segments;
        Fmt.newline ppf ();
        (* Print the messages *)
        pbox ~prefix:pointers (pp_messages ~priority) ppf messages
    in
    loop Column_number.initial "" segments
  ;;

  let pp ~config ~severity ppf t =
    (* Print carets *)
    pp_carets ~config ~severity ppf t;
    (* Print trailing label *)
    pp_trailing_label ~config ~severity ppf t.trailing_segment;
    (* If non-empty, print the hanging segments *)
    if not (List.is_empty t.hanging_segments)
    then Fmt.pf ppf "@.%a" (pp_hanging_segments ~config ~severity) t.hanging_segments
  ;;

  let as_trailing_segment { priority; offset; length; messages } : trailing_segment option
    =
    match messages with
    | [ message ] -> Some { priority; offset; length; message }
    | _ -> None
  ;;

  let of_segments (segments : Line.segment list) : t =
    let rec loop (segments : Line.segment list) accu =
      let rev_segments, offset, prev_segment = accu in
      match segments with
      | [] ->
        (* Determine if the last segment is a trailing segment *)
        (* A trailing segment is defined by:
           - the last segment on the line
           - the span of the label in the trailing segment doesn't intersect any
             other label on the line
        *)
        (match Option.(prev_segment >>= as_trailing_segment) with
         | Some trailing_segment ->
           let hanging_segments = List.rev rev_segments in
           { hanging_segments; trailing_segment = Some trailing_segment }
         | None ->
           let hanging_segments = List.rev (Option.to_list prev_segment @ rev_segments) in
           { hanging_segments; trailing_segment = None })
      | { stag = None; content = _; length } :: segments ->
        (* Segments with no semantic tag cannot be hanging (or trailing) segments *)
        loop segments (rev_segments, Column_number.(add offset length), prev_segment)
      | { stag = Some { priority; inline_labels }; content = _; length } :: segments ->
        let messages =
          (* Ensure that higher priority messages are printed first *)
          inline_labels
          |> List.sort
               ~compare:
                 (Comparable.lift (Comparable.reverse Priority.compare) ~f:Tuple2.get1)
          |> List.map ~f:Tuple2.get2
        in
        loop
          segments
          ( Option.to_list prev_segment @ rev_segments
          , Column_number.add offset length
          , Some { priority; messages; offset; length } )
    in
    loop segments ([], Column_number.initial, None)
  ;;
end

let pp_multi_line_label ~config ~severity ~ctxt ppf multi_line_label =
  Fmt.pf
    ppf
    "@[<h>%*s %a %a@]"
    ctxt.line_num_width
    ""
    (Chars.pp_source_border_left ~config)
    ()
    (Multi_line_label.pp ~config ~severity ~ctxt)
    multi_line_label
;;

let pp_line ~config ~severity ~ctxt ~lnum ppf (line : Line.t) =
  (* Convert segments to inline labels *)
  let inline_labels = Inline_labels.of_segments line.segments in
  (* Render multi-line top in line content if:
     - unique top in multi_line_labels
     - top starts in the whitespace/start of new line *)
  let multi_line_labels, unique_top_multi_line_label =
    List.fold_right
      line.multi_line_labels
      ~init:([], None)
      ~f:(fun multi_line_label (mlls, utmll) ->
        match multi_line_label with
        | Top { start; _ } when (start :> int) - 1 <= line.margin_length ->
          (match utmll with
           | None -> mlls, Some multi_line_label
           | Some utmll -> multi_line_label :: utmll :: mlls, None)
        | _ -> multi_line_label :: mlls, utmll)
  in
  (* print_s
     [%message
      "Mll top"
        (margin_length : int)
        (multi_line_labels : Snippet.Multi_line_label.t list)
        (unique_top_multi_line_label : Snippet.Multi_line_label.t option)]; *)
  (* Print source line (with potential top multi-line label) *)
  Multi_line_label.pp_content_top
    ~ctxt
    ~top:unique_top_multi_line_label
    (pp_source_line ~config ~severity ~ctxt ~lnum)
    ppf
    line;
  (* Print inline labels (if any) *)
  if not (Inline_labels.is_empty inline_labels)
  then (
    Fmt.newline ppf ();
    lbox ~config ~severity ~ctxt (Inline_labels.pp ~config ~severity) ppf inline_labels);
  (* Print multi-line labels (if any) *)
  List.iter multi_line_labels ~f:(fun multi_line_label ->
    Fmt.newline ppf ();
    pp_multi_line_label ~config ~severity ~ctxt ppf multi_line_label)
;;

let pp_locus ~config ~ctxt ~source ppf (line_num, col_num) =
  Fmt.pf
    ppf
    "@[<h>%*s %a %s:%a:%a@]"
    ctxt.line_num_width
    ""
    (Chars.pp_snippet_start ~config)
    ()
    (Option.value (Source.name source) ~default:"unknown")
    Line_number.pp
    line_num
    Column_number.pp
    col_num
;;

let pp_line_gutter ~config ~ctxt ppf () =
  Fmt.pf
    ppf
    "@[<h>%*s %a@]"
    ctxt.line_num_width
    ""
    (Chars.pp_source_border_left ~config)
    ()
;;

let pp_block ~config ~severity ~ctxt ppf ({ start; lines } : Snippet.block) =
  List.iteri lines ~f:(fun i line ->
    if i <> 0 then Fmt.newline ppf ();
    let lnum = Line_number.of_line_index @@ Line_index.add start i in
    pp_line ~config ~severity ~ctxt ~lnum ppf line)
;;

let pp_source
  ~config
  ~severity
  ~line_num_width
  ~multi_width
  ppf
  ({ source; blocks; locus; labels = _ } : Snippet.source)
  =
  let ctxt = { multi_context = Multi_context.create ~len:multi_width; line_num_width } in
  pp_locus ~config ~ctxt ~source ppf locus;
  if not (List.is_empty blocks) then Fmt.pf ppf "@.";
  List.iteri blocks ~f:(fun i block ->
    if i <> 0
    then (
      Fmt.newline ppf ();
      pp_line_break ~config ~severity ~ctxt ppf ());
    pp_block ~config ~severity ~ctxt ppf block)
;;

let pp_header ~config ~severity ppf message =
  Fmt.pf
    ppf
    "@[<h>%a: %a@]"
    (pp_severity ~config)
    severity
    (pp_header_message ~config)
    message
;;

let pp_note = pp_message

let pp_snippet
  ~config
  ~line_num_width
  ~multi_width
  ppf
  Snippet.{ severity; message; sources; notes }
  =
  Fmt.pf ppf "@[<v>%a" (pp_header ~config ~severity) message;
  List.iter sources ~f:(fun file ->
    Fmt.newline ppf ();
    pp_source ~config ~severity ~line_num_width ~multi_width ppf file);
  List.iter notes ~f:(fun note ->
    Fmt.newline ppf ();
    pwibox
      ~prefix:
        (Fmt.str_like ppf "%*s %a " line_num_width "" (Chars.pp_note_bullet ~config) ())
      Message.pp
      ppf
      note);
  Fmt.pf ppf "@]"
;;

let line_num_width (snippet : Snippet.t) =
  Int.max
    (snippet.sources
     |> List.map ~f:(fun { blocks; _ } ->
       let { Snippet.start; lines; _ } = List.last_exn blocks in
       let line_num =
         Line_number.of_line_index Line_index.(add start (List.length lines))
       in
       Line_number.to_string line_num |> String.length)
     |> List.max_elt ~compare:Int.compare
     |> Option.value ~default:0)
    3
;;

let multi_width (snippet : Snippet.t) =
  let rec count_multi : Snippet.Line.t list -> int = function
    | [] -> 0
    | line :: lines ->
      List.count line.multi_line_labels ~f:(function
        | Top _ -> true
        | Bottom _ -> false)
      + count_multi lines
  in
  snippet.sources
  |> List.map ~f:(fun { Snippet.blocks; _ } ->
    let rec loop_blocks : Snippet.block list -> int = function
      | [] -> 0
      | { lines; _ } :: blocks -> count_multi lines + loop_blocks blocks
    in
    loop_blocks blocks)
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0
;;

let pp_diagnostic ~config ppf diagnostic =
  Fmt.set_style_renderer ppf (Config.style_renderer config);
  Format.pp_set_geometry ppf ~max_indent:2 ~margin:Int.max_value;
  Source_reader.with_reader
  @@ fun () ->
  let snippet = Snippet.of_diagnostic diagnostic in
  let line_num_width = line_num_width snippet in
  let multi_width = multi_width snippet in
  (* print_s [%message "Snippet" (snippet : Snippet.t)]; *)
  pp_snippet ~config ~line_num_width ~multi_width ppf snippet
;;
