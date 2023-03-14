open Core
open Grace
open Text

(* open Grace.Text *)
(* This will render a diagnostic to a snippet (to forms, rich and simple) *)

(* Process for rich diagnostics:
    1. group into files
    2. determine locus of labels in a given file
    3. determine gutters (max # of gutters)
    4. group file by "labelled lines".
    5. labelled lines into snippet source lines + single label + multi labels
    6. Profit
*)

(* Warning!!! This code is spagetti *)

module Rich = struct
  open Diagnostic

  module Line = struct
    type t =
      { line : Line_index.t (* range is range of error messages. Includes all of them. *)
      ; range : Range.Column_index.t
      ; single_labels : (Priority.t * Range.Column_index.t * Label.t) list
      ; multi_labels : (int * Priority.t * multi_label) list
      }

    and multi_label =
      [ `Top of Column_index.t
      | `Verticle
      | `Bottom of Column_index.t * Label.t
      ]

    let to_snippet
        ~files
        ~file_id
        ~max_num_multiline_labels
        { line; range; single_labels; multi_labels }
      =
      (* TODO: Prettify this code in Grace harderning *)
      let module Column_span = Span.Column_number in
      (* 1. Handle unicode support by mapping indices into text locations (using [source_line]) *)
      let source_line =
        File.Cache.(source_slice files file_id (line_range files file_id line))
      in
      Fmt.pr "Source line: %s\n" source_line;
      let margin_length =
        Text.length source_line - Text.length (Text.lstrip source_line)
      in
      let span = Column_span.of_range range ~line:source_line in
      let single_labels =
        List.map single_labels ~f:(fun (priority, range, label) ->
            priority, Column_span.of_range range ~line:source_line, label)
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
              | `Verticle -> `Verticle ))
      in
      (* Snippet converters *)
      let open Snippet in
      let gutters =
        let gutters = Array.create ~len:max_num_multiline_labels None in
        List.iter multi_labels ~f:(fun (idx, priority, label) ->
            let gutter =
              match label with
              | `Top start when start - 1 <= margin_length -> `Top
              | _ -> `Verticle
            in
            gutters.(idx) <- Some (gutter, priority));
        Array.to_list gutters
      in
      let vert_gutters =
        let gutters = Array.create ~len:max_num_multiline_labels None in
        List.iter multi_labels ~f:(fun (idx, priority, _) ->
            gutters.(idx) <- Some priority);
        Array.to_list gutters
      in
      (* 2. Annotate the source line with priority information *)
      let annotated_source_line =
        List.mapi
          (Text.explode (Text.rstrip source_line))
          ~f:(fun column_number chr ->
            let column_number = column_number + 1 in
            let priority =
              List.filter_map single_labels ~f:(fun (priority, span, _) ->
                  Option.some_if (Column_span.contains span column_number) priority)
              @ List.filter_map multi_labels ~f:(fun (_, priority, label) ->
                    Option.some_if
                      (match label with
                      | `Top start -> start <= column_number
                      | `Bottom (stop, _) -> column_number < stop
                      | `Verticle -> false)
                      priority)
              |> List.max_elt ~compare:Priority.compare
            in
            chr, priority)
      in
      (* 3. Create [Source_line] snippet *)
      let source_line_snippet =
        Source_line.
          { line = Line_number.of_index line; gutters; source = annotated_source_line }
      in
      (* 4. Create [Single_label] snippet *)
      let single_label_snippet =
        match single_labels with
        | [] -> []
        | single_labels ->
          (* Approach for finding the trailing label:
              1. Iterate through single labels finding 
                 the rightmost label
              2. Check this label doesn't intersect with any other label 
          *)
          (* TODO: Implement improved algorithm here (by tracking max start,stop col numbers) *)
          let trailing_label =
            List.foldi single_labels ~init:None ~f:(fun i trailing_label label ->
                match trailing_label with
                | None -> Some (i, label)
                | Some trailing_label ->
                  let _, (_, tspan, _) = trailing_label in
                  let _, span, _ = label in
                  Some
                    (if Column_span.stop tspan < Column_span.stop span
                    then i, label
                    else trailing_label))
          in
          let trailing_label =
            match trailing_label with
            | None -> None
            | Some ((_, (_, tspan, _)) as tlabel) ->
              Option.some_if
                (List.for_all single_labels ~f:(fun (_, span, _) ->
                     Column_span.are_disjoint span tspan))
                tlabel
          in
          let hanging_labels =
            List.filteri single_labels ~f:(fun i _ ->
                Option.value_map ~default:true trailing_label ~f:(fun (j, _) -> i <> j))
          in
          let carets =
            List.mapi
              (Text.explode (Text.rstrip source_line))
              ~f:(fun column_number _chr ->
                let column_number = column_number + 1 in
                let priority =
                  List.filter_map single_labels ~f:(fun (priority, span, _) ->
                      Option.some_if (Column_span.contains span column_number) priority)
                  |> List.max_elt ~compare:Priority.compare
                in
                priority)
          in
          [ Single_label.
              { gutters = vert_gutters
              ; column_span = span
              ; carets
              ; trailing_label = Option.map trailing_label ~f:snd
              ; hanging_labels
              }
          ]
      in
      (* 5. Create [Multi_label] snippets *)
      let multi_label_snippets =
        let vert_gutters =
          vert_gutters
          |> List.map ~f:(Option.map ~f:(fun priority -> `Verticle, priority))
          |> Array.of_list
        in
        List.filter_map multi_labels ~f:(fun (idx, priority, label) ->
            match label with
            | `Top start when start - 1 <= margin_length -> None
            | `Verticle -> None
            | `Top start ->
              let vert_gutters = Array.copy vert_gutters in
              vert_gutters.(idx) <- Some (`Top, priority);
              let gutters = Array.to_list vert_gutters in
              let kind = Multi_label.Top start in
              Some Multi_label.{ gutters; gutter_idx = idx; priority; kind }
            | `Bottom (stop, label) ->
              let vert_gutters = Array.copy vert_gutters in
              vert_gutters.(idx) <- Some (`Bottom, priority);
              let gutters = Array.to_list vert_gutters in
              let kind = Multi_label.Bottom (stop - 1, label) in
              Some Multi_label.{ gutters; gutter_idx = idx; priority; kind })
      in
      Snippet_line.(
        [ Source_line source_line_snippet ]
        @ List.map ~f:(fun t -> Single_label t) single_label_snippet
        @ List.map ~f:(fun t -> Multi_label t) multi_label_snippets)
    ;;
  end

  module File = struct
    let of_labels ~files ~file_id (labels : Label.t list) =
      (* Iterate through labels:
          1. determine locus (i.e. first location of error )
          2. group labels into "lines":
              A line is a line number, the range within the line
              the single labels in the line and the multi labels
              non-consecutive splits in lines should have a break rendered as well (gutters dependent on multi-labels of next line)
          3. render lines *)
      let lines : (Line_index.t, Line.t) Hashtbl.t = Hashtbl.create (module Line_index) in
      let max_num_multiline_labels = ref 0 in
      List.iter labels ~f:(fun label ->
          let range = label.range in
          Fmt.pr "Range: %a\n" Range.pp range;
          let start_line = File.Cache.line_index files file_id (Range.start range)
          and end_line = File.Cache.line_index files file_id (Range.stop range) in
          Fmt.pr "Line indices: %a, %a\n" Line_index.pp start_line Line_index.pp end_line;
          let start_line_range = File.Cache.line_range files file_id start_line
          and end_line_range = File.Cache.line_range files file_id end_line in
          if Line_index.(start_line = end_line)
          then (
            (* Single label *)
            print_endline "Adding single label";
            let offset = max ((Range.stop range :> int) - (Range.start range :> int)) 1 in
            Fmt.pf Format.std_formatter "start_column_index: %d\n"
            @@ ((Range.start range :> int) - (Range.start start_line_range :> int));
            let start_column_index =
              Column_index.create
              @@ ((Range.start range :> int) - (Range.start start_line_range :> int))
            in
            let range =
              Range.Column_index.create
                start_column_index
                Column_index.(start_column_index + offset)
            in
            let label = label.priority, range, label in
            Hashtbl.update lines start_line ~f:(function
                | None ->
                  Line.
                    { line = start_line
                    ; range
                    ; single_labels = [ label ]
                    ; multi_labels = []
                    }
                | Some { line; range = range'; single_labels; multi_labels } ->
                  Line.
                    { line
                    ; range = Range.Column_index.merge range range'
                    ; single_labels = label :: single_labels
                    ; multi_labels
                    }))
          else (
            (* Multi label *)
            print_endline "Adding multi label";
            let idx = !max_num_multiline_labels in
            Int.incr max_num_multiline_labels;
            (* Adding line above top *)
            if Line_index.(start_line > initial)
            then (
              let line = Line_index.(start_line - 1) in
              let range = File.Cache.line_column_range files file_id line in
              Hashtbl.update lines line ~f:(function
                  | None -> Line.{ line; range; single_labels = []; multi_labels = [] }
                  | Some { line; range = range'; single_labels; multi_labels } ->
                    { line
                    ; range = Range.Column_index.merge range range'
                    ; single_labels
                    ; multi_labels
                    }));
            print_endline "Adding top";
            (* Add top *)
            let start_offset =
              Column_index.create
              @@ ((Range.start range :> int) - (Range.start start_line_range :> int))
            in
            let start_range =
              Range.Column_index.create Column_index.initial start_offset
            in
            let top_label = idx, label.priority, `Top start_offset in
            Hashtbl.update lines start_line ~f:(function
                | None ->
                  Line.
                    { line = start_line
                    ; range = start_range
                    ; single_labels = []
                    ; multi_labels = [ top_label ]
                    }
                | Some { line; range = range'; single_labels; multi_labels } ->
                  { line
                  ; range = Range.Column_index.merge start_range range'
                  ; single_labels
                  ; multi_labels = top_label :: multi_labels
                  });
            (* Adding line below bottom *)
            if Line_index.(end_line < File.Cache.last_line_index files file_id)
            then (
              let line = Line_index.(end_line + 1) in
              let range = File.Cache.line_column_range files file_id line in
              Hashtbl.update lines line ~f:(function
                  | None -> Line.{ line; range; single_labels = []; multi_labels = [] }
                  | Some { line; range = range'; single_labels; multi_labels } ->
                    { line
                    ; range = Range.Column_index.merge range range'
                    ; single_labels
                    ; multi_labels
                    }));
            (* Add bottom *)
            print_endline "Adding bottom";
            let end_offset =
              Column_index.create
              @@ ((Range.stop range :> int) - (Range.start end_line_range :> int))
            in
            let end_range = Range.Column_index.create Column_index.initial end_offset in
            let bottom_label = idx, label.priority, `Bottom (end_offset, label) in
            Hashtbl.update lines end_line ~f:(function
                | None ->
                  Line.
                    { line = end_line
                    ; range = end_range
                    ; single_labels = []
                    ; multi_labels = [ bottom_label ]
                    }
                | Some { line; range = range'; single_labels; multi_labels } ->
                  { line
                  ; range = Range.Column_index.merge end_range range'
                  ; single_labels
                  ; multi_labels = bottom_label :: multi_labels
                  });
            print_endline "Adding verts";
            (* Add verticles *)
            for i = (start_line :> int) + 1 to (end_line :> int) - 1 do
              let line = Line_index.create i in
              let line_range = File.Cache.line_column_range files file_id line in
              let verticle_label = idx, label.priority, `Verticle in
              Hashtbl.update lines line ~f:(function
                  | None ->
                    Line.
                      { line
                      ; range = line_range
                      ; single_labels = []
                      ; multi_labels = [ verticle_label ]
                      }
                  | Some { line; range = range'; single_labels; multi_labels } ->
                    { line
                    ; range = Range.Column_index.merge range' line_range
                    ; single_labels
                    ; multi_labels = verticle_label :: multi_labels
                    })
            done));
      let lines =
        Hashtbl.to_alist lines
        |> List.sort ~compare:(Comparable.lift Line_index.compare ~f:fst)
      in
      let file_name = File.Cache.name files file_id in
      let loc =
        let _start_line = List.hd_exn lines in
        (* TODO: fix *)
        let idx = Byte_index.initial in
        File.Cache.location files file_id idx
      in
      let locus = Snippet.Locus.{ file_name; loc } in
      let lines =
        List.concat_map lines ~f:(fun (_, line) ->
            Line.to_snippet
              ~files
              ~file_id
              ~max_num_multiline_labels:!max_num_multiline_labels
              line)
      in
      Snippet.File.{ locus; lines }
    ;;
  end
end

let rich ~files (diagnostic : Diagnostic.t) =
  let Diagnostic.{ severity; message; labels; notes } = diagnostic in
  let labels_per_file =
    List.sort_and_group labels ~compare:(fun label1 label2 ->
        File.Id.compare label1.id label2.id)
  in
  print_endline "Grouped labels per file!";
  let files =
    List.map
      ~f:(fun labels ->
        let file_id = (List.hd_exn labels : Diagnostic.Label.t).id in
        Rich.File.of_labels ~files ~file_id labels)
      labels_per_file
  in
  Snippet.{ severity; message; files; notes }
;;

let simple ~files:_ (_diagnostic : Diagnostic.t) = assert false
