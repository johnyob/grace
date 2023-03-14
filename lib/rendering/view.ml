open Core
open Grace
module Text = Grace_text
open Text

module type S = sig
  type t

  val to_snippet : ?debug:bool -> t -> Diagnostic.t -> files:Files.t -> Snippet.t
end

module Rich = struct
  open Diagnostic

  type t = unit

  module Line = struct
    module Column_span = Span.Column_number

    type t =
      { line : Line_index.t
      ; single_labels : (Range.Column_index.t * Label.t) list
      ; multi_labels :
          (int
          * Priority.t
          * [ `Top of Column_index.t | `Vertical | `Bottom of Column_index.t * Label.t ])
          list
      }
    [@@deriving sexp]

    let trailing_label single_labels =
      (* A trialing label is defined as a label that
          - the last label on the line
          - the span of the trailing label doesn't intersect any other label on the line
      *)
      (* TODO: Implement improved algorithm here (by tracking max start,stop col numbers) 
         to avoid a double pass on single labels *)
      let trailing_label =
        List.foldi single_labels ~init:None ~f:(fun i trailing_label label ->
            match trailing_label with
            | None -> Some (i, label)
            | Some trailing_label ->
              let _, (tspan, _) = trailing_label in
              let span, _ = label in
              Some
                (if Column_span.stop tspan < Column_span.stop span
                then i, label
                else trailing_label))
      in
      let trailing_label =
        match trailing_label with
        | None -> None
        | Some (idx, (tspan, label)) ->
          Option.some_if
            (List.for_alli single_labels ~f:(fun idx' (span, _) ->
                 (* All labels (excluding trailing label) must not overlap with trailing label *)
                 idx = idx' || Column_span.are_disjoint span tspan))
            (idx, label)
      in
      match trailing_label with
      | None -> single_labels, None
      | Some (trailing_idx, label) ->
        let single_labels =
          List.filteri single_labels ~f:(fun i _ -> i <> trailing_idx)
        in
        single_labels, Some label
    ;;

    let vert_marks multi_labels =
      List.map multi_labels ~f:(fun (idx, priority, _) ->
          Snippet.Mark.{ idx; priority; kind = `Vertical })
    ;;

    let to_snippet_lines
        ?(debug = false)
        ({ line; single_labels; multi_labels } as t)
        ~files
        ~file_id
      =
      if debug then Fmt.pr "@[Line: %a@]@." Sexp.pp_hum (sexp_of_t t);
      let open Snippet in
      (* 1. Handle unicode support by mapping indices into text locations (using [source_line]) *)
      let line_range =
        Option.value_exn ~here:[%here] @@ Files.Line.range files file_id line
      in
      let source_line = Files.(Source.slice files file_id line_range) in
      let margin_length =
        Text.length source_line - Text.length (Text.lstrip source_line)
      in
      let single_labels =
        List.map single_labels ~f:(fun (range, label) ->
            Column_span.of_range range ~line:source_line, label)
      in
      let multi_labels =
        List.map multi_labels ~f:(fun (idx, priority, label) ->
            ( idx
            , priority
            , match label with
              | `Top start_idx ->
                `Top (Column_number.of_index start_idx ~line:source_line)
              | `Bottom (stop_idx, label) ->
                `Bottom (Column_number.of_index stop_idx ~line:source_line, label)
              | `Vertical -> `Vertical ))
      in
      (* 2. Create [marks] *)
      let vert_marks =
        List.filter_map multi_labels ~f:(fun (idx, priority, label) ->
            match label with
            | `Top start when start - 1 <= margin_length ->
              Some Mark.{ idx; priority; kind = `Vertical }
            | `Top _ -> None
            | _ -> Some Mark.{ idx; priority; kind = `Vertical })
      in
      let marks =
        List.filter_map multi_labels ~f:(fun (idx, priority, label) ->
            match label with
            | `Top start when start - 1 <= margin_length ->
              Some Mark.{ idx; priority; kind = `Top }
            | `Top _ -> None
            | _ -> Some Mark.{ idx; priority; kind = `Vertical })
      in
      (* 3. Create [Content] snippet *)
      let annotated_source_line =
        List.mapi
          (Text.explode (Text.rstrip source_line))
          ~f:(fun idx chr ->
            let column_number = idx + 1 in
            let priority =
              List.filter_map single_labels ~f:(fun (span, label) ->
                  Option.some_if (Column_span.contains span column_number) label.priority)
              @ List.filter_map multi_labels ~f:(fun (_, priority, label) ->
                    Option.some_if
                      (match label with
                      | `Top start -> start <= column_number
                      | `Bottom (stop, _) -> column_number < stop
                      | `Vertical -> false)
                      priority)
              |> List.max_elt ~compare:Priority.compare
            in
            chr, priority)
      in
      let content_snippet =
        Line.Source
          { marks
          ; line =
              Content { line = Line_number.of_index line; source = annotated_source_line }
          }
      in
      (* 4. Create single label snippets *)
      let hanging_labels, single_label_snippets =
        match single_labels with
        | [] -> [], []
        | single_labels ->
          (* 1. Determine trailing label (if it exists) *)
          let hanging_labels, trailing_label = trailing_label single_labels in
          (* 2. Create [Single_label] snippet *)
          let carets =
            List.filter_mapi
              (Text.explode (Text.rstrip source_line))
              ~f:(fun column_idx _chr ->
                let column_num = column_idx + 1 in
                let priority =
                  List.filter_map single_labels ~f:(fun (span, label) ->
                      Option.some_if (Column_span.contains span column_num) label.priority)
                  |> List.max_elt ~compare:Priority.compare
                in
                Option.map priority ~f:(fun priority -> column_num, priority))
          in
          let single_label =
            Line.Source
              { marks = vert_marks; line = Single_label { carets; trailing_label } }
          in
          hanging_labels, [ single_label ]
      in
      (* 5. Create hanging label snippets *)
      let hanging_label_snippets =
        match hanging_labels with
        | [] -> []
        | hanging_labels ->
          (* 1. Sort hanging labels by [Column_span.start]  *)
          let hanging_labels =
            List.sort
              hanging_labels
              ~compare:
                Comparable.(
                  lift Column_number.compare ~f:(fun (span, _) -> Column_span.start span))
          in
          (* 2. Create [Caret_pointers] *)
          let caret_pointers =
            Line.Source
              { marks = vert_marks
              ; line =
                  Caret_pointers
                    (List.map hanging_labels ~f:(fun (span, label) ->
                         span, label.priority))
              }
          in
          (* 3. Create [Hanging_label]s *)
          let add_caret_pointer pointer hanging_labels =
            List.map hanging_labels ~f:(fun hanging_label ->
                let pointer_start = Column_span.start (fst pointer) in
                if pointer_start < hanging_label.Hanging_label.label_start
                then
                  Hanging_label.
                    { hanging_label with pointers = pointer :: hanging_label.pointers }
                else hanging_label)
          in
          let hanging_labels =
            hanging_labels
            |> List.fold_right ~init:[] ~f:(fun (span, label) hanging_labels ->
                   Hanging_label.
                     { pointers = []; label_start = Column_span.start span; label }
                   :: add_caret_pointer (span, label.priority) hanging_labels)
            |> List.map ~f:(fun hanging_label ->
                   Line.Source { marks = vert_marks; line = Hanging_label hanging_label })
            |> List.rev
          in
          caret_pointers :: hanging_labels
      in
      (* 6. Create [Multi_label] snippets *)
      let multi_label_snippets =
        (* Assumes [multi_labels] is sorted by idx. *)
        let marks ~mark_idx ~mark_kind =
          List.filter_map multi_labels ~f:(fun (idx, priority, kind) ->
              if mark_idx = idx
              then Some Mark.{ idx; priority; kind = mark_kind }
              else (
                match kind with
                | `Top _ when mark_idx < idx ->
                  (* If a `Top mark is yet to be rendered *)
                  None
                | `Bottom _ when idx < mark_idx ->
                  (* If a `Bottom mark has been rendered *)
                  None
                | _ -> Some Mark.{ idx; priority; kind = `Vertical }))
        in
        List.filter_map multi_labels ~f:(fun (idx, priority, label) ->
            match label with
            | `Top start when start - 1 <= margin_length -> None
            | `Vertical -> None
            | `Top start ->
              Some
                (Line.Source
                   { marks = marks ~mark_idx:idx ~mark_kind:`Top
                   ; line = Multi_label { mark_idx = idx; priority; kind = `Top start }
                   })
            | `Bottom (stop, label) ->
              Some
                (Line.Source
                   { marks = marks ~mark_idx:idx ~mark_kind:`Bottom
                   ; line =
                       Multi_label
                         { mark_idx = idx; priority; kind = `Bottom (stop - 1, label) }
                   }))
      in
      (content_snippet :: single_label_snippets)
      @ hanging_label_snippets
      @ multi_label_snippets
    ;;
  end

  module File = struct
    type t =
      { id : File.Id.t
      ; locus : Byte_index.t
      ; labels : Label.t list
      }

    let locus ~files id locus : Snippet.Locus.t =
      let line = Files.Line.index files id locus in
      let line_start = Option.value_exn ~here:[%here] @@ Files.Line.start files id line in
      let line_source =
        Option.value_exn ~here:[%here] @@ Files.Line.slice files id line
      in
      { file_name = Files.name files id
      ; position =
          Position.
            { line = Line_number.of_index line
            ; column =
                Column_number.of_index ~line:line_source
                @@ Column_index.create Byte_index.(locus -. line_start)
            }
      }
    ;;

    let to_snippet_lines ?debug { id; locus = locus'; labels } ~files =
      (* Create locus *)
      let locus = Snippet.Line.Raw (Locus (locus ~files id locus')) in
      (* Split labels into [single_labels] and [multi_labels] *)
      let single_labels, multi_labels =
        List.partition_map labels ~f:(fun label ->
            let start_line = Files.Line.index files id (Range.start label.range)
            and end_line = Files.Line.index files id (Range.stop label.range) in
            if Line_index.(start_line = end_line)
            then First (start_line, label)
            else Second (start_line, end_line, label))
      in
      (* A line is to be rendered if:
          - It contains a single label
          - It is withing +-1 lines of a multi-line top or bottom (so-called contextual lines)
          - It is between two other rendered lines (also contextual lines)
      *)
      (* TODO: The last criteria is handled by the [loop] function. This should be refactored. *)
      let last_line = Files.Line.last files id in
      let contextual_lines (line : Line_index.t) =
        let line = (line :> int) in
        [ line - 1; line; line + 1 ]
        |> List.filter ~f:(fun line -> 0 <= line && line < (last_line :> int))
        |> List.map ~f:Line_index.create
      in
      (* Get rendered lines (sorted) *)
      let lines : Line_index.t Hash_set.t = Hash_set.create (module Line_index) in
      List.iter single_labels ~f:(fun (line, _) -> Hash_set.add lines line);
      multi_labels
      |> List.concat_map ~f:(fun (start_line, end_line, _) ->
             contextual_lines start_line @ contextual_lines end_line)
      |> List.iter ~f:(fun line -> Hash_set.add lines line);
      let lines = Hash_set.to_list lines |> List.sort ~compare:Line_index.compare in
      (* Iterate through [lines] creating a [Line.t] representation for each index *)
      let lines =
        List.map lines ~f:(fun line ->
            let line_range =
              Option.value_exn ~here:[%here] @@ Files.Line.range files id line
            in
            let single_labels =
              List.filter_map single_labels ~f:(fun (line_idx', label) ->
                  if Line_index.(line_idx' = line)
                  then (
                    let range = label.range in
                    (* Calculate the column range of the label *)
                    (* [offset] is a minimum of 1 (to account for labels with no range) *)
                    let offset =
                      max Byte_index.(Range.stop range -. Range.start range) 1
                    in
                    (* [start_column_index] is the offset between the line start and the label range *)
                    let start_column_index =
                      Column_index.create
                      @@ Byte_index.(Range.start range -. Range.start line_range)
                    in
                    let column_range =
                      Range.Column_index.create
                        start_column_index
                        Column_index.(start_column_index + offset)
                    in
                    Some (column_range, label))
                  else None)
            in
            let multi_labels =
              List.filter_mapi multi_labels ~f:(fun idx (start_line, end_line, label) ->
                  let priority = label.priority in
                  let range = label.range in
                  if Line_index.(line = start_line)
                  then (
                    (* [Top] multi-label *)
                    let start_offset =
                      Column_index.create
                      @@ Byte_index.(Range.start range -. Range.start line_range)
                    in
                    Some (idx, priority, `Top start_offset))
                  else if Line_index.(line = end_line)
                  then (
                    (* [Bottom] multi-label *)
                    let end_offset =
                      Column_index.create
                      @@ Byte_index.(Range.stop range -. Range.start line_range)
                    in
                    Some (idx, priority, `Bottom (end_offset, label)))
                  else if Line_index.(start_line < line && line < end_line)
                  then (* [Vertical] multi-label *)
                    Some (idx, priority, `Vertical)
                  else None)
            in
            Line.{ line; single_labels; multi_labels })
      in
      (* Iterate through lines:
          1. Handling breaks / contextual lines
          2. render line using [Line.to_snippet_lines]
      *)
      let rec loop lines =
        match lines with
        | [] -> []
        | [ line ] -> Line.to_snippet_lines ?debug ~files ~file_id:id line
        | line :: next_line :: lines ->
          let line_delta = Line_index.(next_line.line -. line.line) in
          let marks =
            lazy
              (line.multi_labels
              |> List.filter ~f:(function
                     | _, _, `Bottom _ -> false
                     | _ -> true)
              |> Line.vert_marks)
          in
          Line.to_snippet_lines ?debug ~files ~file_id:id line
          @ (if line_delta <= 0
            then (* Impossible. Invariant [next_line.line > line.line] *)
              assert false
            else if line_delta = 1
            then (* Consecutive lines *)
              []
            else if line_delta = 2
            then (
              (* A single line gap between the lines. Here we add a "contextual" line *)
              let ctxt_line = Line_index.(line.line + 1) in
              let ctxt_source =
                Files.(
                  Source.slice files id
                  @@ Option.value_exn ~here:[%here]
                  @@ Line.range files id ctxt_line)
              in
              Snippet.Line.
                [ Source
                    { marks = Lazy.force marks
                    ; line =
                        Content
                          { line = Line_number.of_index ctxt_line
                          ; source = [ ctxt_source, None ]
                          }
                    }
                ])
            else
              (* [line_delta > 2]. Add a break *)
              Snippet.Line.[ Source { marks = Lazy.force marks; line = Break } ])
          @ loop (next_line :: lines)
      in
      locus :: loop lines
    ;;
  end
end

let rich ?debug ~files (diagnostic : Diagnostic.t) =
  let Diagnostic.{ severity; message; labels; notes } = diagnostic in
  let labels_per_file =
    List.sort_and_group labels ~compare:(fun label1 label2 ->
        File.Id.compare label1.id label2.id)
  in
  let lines =
    List.concat_map
      ~f:(fun labels ->
        let file_id = (List.hd_exn labels : Diagnostic.Label.t).id in
        let _, locus =
          (* The locus is defined as the earliest highest priority position in the the set of labels *)
          labels
          |> List.map ~f:(fun label -> label.priority, Range.start label.range)
          |> List.max_elt ~compare:[%compare: Diagnostic.Priority.t * Byte_index.t]
          |> Option.value_exn ~here:[%here]
        in
        Rich.File.to_snippet_lines ?debug { id = file_id; locus; labels } ~files)
      labels_per_file
  in
  Snippet.
    { severity
    ; lines =
        Line.(
          [ Raw (Title { locus = None; title = message }) ]
          @ lines
          @ [ Raw (Notes notes) ])
    }
;;

let compact ~files (diagnostic : Diagnostic.t) =
  let open Diagnostic in
  let { severity; message; labels; notes = _ } = diagnostic in
  let primary_loci =
    List.filter_map labels ~f:(fun { priority; range; id; _ } ->
        match priority with
        | Secondary -> None
        | Primary -> Some (Rich.File.locus ~files id (Range.start range)))
  in
  let lines =
    match primary_loci with
    | [] -> [ Snippet.Line.Raw (Title { locus = None; title = message }) ]
    | primary_loci ->
      List.map primary_loci ~f:(fun locus ->
          Snippet.Line.Raw (Title { locus = Some locus; title = message }))
  in
  Snippet.{ severity; lines }
;;
