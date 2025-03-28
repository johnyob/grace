open! Import
open! Diagnostic
module Source_descr = Source_reader.Source_descr

let margin_length_of_string line_content =
  (* This is valid for UTF8 as all the whitespace characters we're
     interested in wrt a 'margin' have a width of 1. *)
  String.length line_content - String.length (String.lstrip line_content)
;;

module Utf8 = struct
  let length s =
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
    let rec loop acc =
      match Uutf.decode decoder with
      | `Uchar _ -> loop (acc + 1)
      | `End -> acc
      | `Malformed _ -> raise (Invalid_argument "invalid UTF-8")
      | `Await -> assert false
    in
    loop 0
  ;;
end

module type Number = Index

module Int_number = struct
  module T = struct
    type t = int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let invariant t = Invariant.invariant [%here] t sexp_of_t (fun () -> assert (t >= 1))
  let pp = Format.pp_print_int
  let to_string = Int.to_string

  let of_int t =
    invariant t;
    t
  ;;

  let initial = 1

  let add t off =
    let t = t + off in
    invariant t;
    t
  ;;

  let sub t off =
    let t = t - off in
    invariant t;
    t
  ;;

  let diff t1 t2 = t1 - t2
end

module Line_number = struct
  include Int_number

  let of_line_index (idx : Line_index.t) = (idx :> int) + 1
end

module Column_number = struct
  include Int_number

  let of_byte_index (idx : Byte_index.t) ~sd ~line =
    let content = Source_reader.(slicei sd (Line.start line) idx) in
    let length = Utf8.length content in
    length + 1
  ;;
end

module Multi_line_label = struct
  module Id = Int

  type t =
    | Top of
        { id : Id.t
        ; start : Column_number.t
        ; priority : Priority.t
        }
    | Bottom of
        { id : Id.t
        ; stop : Column_number.t
        ; priority : Priority.t
        ; label : Message.t
        }
  [@@deriving sexp]
end

module Line = struct
  type stag =
    { priority : Priority.t
    ; inline_labels : (Priority.t * Message.t) list
    }

  and segment =
    { content : string
    ; length : int
    ; stag : stag option
    }

  and t =
    { segments : segment list
    ; multi_line_labels : Multi_line_label.t list
    ; margin_length : int
    }
  [@@deriving sexp]
end

type block =
  { start : Line_index.t
  ; lines : Line.t list
  }

and source =
  { source : Source.t
  ; locus : locus
  ; blocks : block list
  }

and locus = Line_number.t * Column_number.t

and sources =
  | Rich of source list
  | Compact of (Source.t * locus) list

and 'code t =
  { severity : Severity.t
  ; message : Message.t
  ; code : 'code option
  ; sources : sources
  ; notes : Message.t list
  }
[@@deriving sexp]

let locus_of_labels ~sd (labels : Label.t list) =
  (* The locus is defined as the earliest highest priority position in the the set of labels *)
  let _, locus_idx =
    labels
    |> List.map ~f:(fun label -> label.priority, Range.start label.range)
    |> List.max_elt ~compare:[%compare: Diagnostic.Priority.t * Byte_index.t]
    |> Option.value_exn ~here:[%here]
  in
  let line = Source_reader.Line.of_byte_index sd locus_idx in
  Line_number.of_line_index line.idx, Column_number.of_byte_index locus_idx ~sd ~line
;;

let group_labels_by_source labels =
  labels
  |> List.sort_and_group
       ~compare:
         (Comparable.lift Source.compare ~f:(fun (label : Label.t) ->
            Range.source label.range))
  |> List.map ~f:(fun labels ->
    (* Invariants:
       + [List.length labels > 0]
       + Sources for each label are equal *)
    let source = Range.source (List.hd_exn labels).range in
    source, labels)
;;

module Of_diagnostic = struct
  type 'a with_line =
    { line : Line_index.t
    ; it : 'a
    }

  let range_of_labels = function
    | [] -> assert false
    | { Label.range; _ } :: _ as labels ->
      List.fold_left
        labels
        ~f:(fun range label -> Range.merge range label.range)
        ~init:range
  ;;

  let line_of_range ~sd (line : Source_reader.Line.t) =
    let content = Source_reader.Line.slice ~sd line in
    { line = line.idx
    ; it =
        Line.
          { segments = [ { content; length = Utf8.length content; stag = None } ]
          ; multi_line_labels = []
          ; margin_length = margin_length_of_string content
          }
    }
  ;;

  let line_of_idx ~sd (line_idx : Line_index.t) =
    line_of_range ~sd @@ Source_reader.Line.of_line_index sd line_idx
  ;;

  module Priority_count = struct
    type t =
      { primary : int
      ; secondary : int
      }

    let zero = { primary = 0; secondary = 0 }

    let start t ~priority : t =
      match priority with
      | Priority.Primary -> { t with primary = t.primary + 1 }
      | Secondary -> { t with secondary = t.secondary + 1 }
    ;;

    let stop t ~priority : t =
      match priority with
      | Priority.Primary ->
        assert (t.primary > 0);
        { t with primary = t.primary - 1 }
      | Secondary ->
        assert (t.secondary > 0);
        { t with secondary = t.secondary - 1 }
    ;;

    let priority = function
      | { primary = 0; secondary = 0 } -> None
      | { primary = 0; secondary = _ } -> Some Priority.Secondary
      | { primary = _; secondary = _ } -> Some Primary
    ;;
  end

  let add_eol_segment ~sd ~(line : Source_reader.Line.t) cursor rev_segments =
    let stop = Source_reader.Line.stop line in
    if (* If [cursor < stop -1], then we require a non-empty end-of-line segment *)
       Byte_index.(add cursor 1 < stop)
    then
      Line.
        { content = Source_reader.slicei sd cursor stop
        ; length = Byte_index.(diff stop cursor)
        ; stag = None
        }
      :: rev_segments
    else rev_segments
  ;;

  let end_segments ~sd ~(line : Source_reader.Line.t) cursor rev_segments =
    rev_segments |> add_eol_segment ~sd ~line cursor |> List.rev
  ;;

  let segments_of_labels ~sd ~(line : Source_reader.Line.t) (labels : Label.t list) =
    (* 1. Convert labels into a interval set consisting of start and stop points *)
    let points =
      let compare_point =
        (* We lexicographically compare the idx and then we order starting points before stopping points *)
        Comparable.lift
          [%compare: Byte_index.t * [ `Start | `Stop ]]
          ~f:(fun (idx, _, start_or_stop) ->
            let start_or_stop =
              match start_or_stop with
              | `Start _ -> `Start
              | `Stop -> `Stop
            in
            idx, start_or_stop)
      in
      labels
      |> List.concat_map ~f:(fun Label.{ range; priority; message } ->
        let start, stop = Range.split range in
        [ start, priority, `Start message; stop, priority, `Stop ])
      |> List.sort ~compare:compare_point
    in
    let rev_segments, _, cursor, cursor_labels =
      (* 2. Iterate through the interval set, maintaining a:
         + List of (reversed) segments
         + A priority counter -- a cheap way of deducing the priority of the cursor
         + A cursor -- the current byte position we're in the interval set
         + A list of labels starting at the cursor ('cursor labels')
      *)
      let eol = Source_reader.Line.stop line in
      List.fold_left
        points
        ~init:([], Priority_count.zero, Source_reader.Line.start line, [])
        ~f:
          (fun
            (rev_segments, priority_count, cursor, cursor_labels)
            (idx, priority, start_or_stop)
          ->
          (* If the next point is at the current cursor and the cursor is before the
             end of the line ... *)
          if Byte_index.(cursor = idx) && Byte_index.(idx < eol)
          then (
            let priority_count, cursor_msgs =
              match start_or_stop with
              | `Start msg ->
                (* and the point defined a start of a message, we increment the priority counter
                   and add the label to the cursor labels. *)
                ( Priority_count.start priority_count ~priority
                , (priority, msg) :: cursor_labels )
              | `Stop ->
                (* and the point defined a end of a message, we decrement the priority counter. *)
                Priority_count.stop priority_count ~priority, cursor_labels
            in
            rev_segments, priority_count, cursor, cursor_msgs)
          else (
            (* otherwise, we create a segment from 'cursor' to 'idx' and set 'cursor' to 'idx' *)
            let content = Source_reader.slicei sd cursor idx in
            let segment =
              Line.
                { content
                ; length = Utf8.length content
                ; stag =
                    Option.(
                      Priority_count.priority priority_count
                      >>| fun priority -> { priority; inline_labels = cursor_labels })
                }
            in
            let priority_count, cursor_labels =
              match start_or_stop with
              | `Start msg ->
                Priority_count.start priority_count ~priority, [ priority, msg ]
              | `Stop -> Priority_count.stop priority_count ~priority, []
            in
            segment :: rev_segments, priority_count, idx, cursor_labels))
    in
    assert (List.is_empty cursor_labels);
    (* 3. Add end-of-line segment to end the line *)
    let segments = end_segments ~sd ~line cursor rev_segments in
    segments
  ;;

  let line_of_labels ~sd ~line labels multi_line_labels =
    let segments = segments_of_labels ~sd ~line labels in
    { line = line.idx
    ; it =
        Line.
          { segments
          ; multi_line_labels
          ; margin_length = margin_length_of_string (Source_reader.Line.slice ~sd line)
          }
    }
  ;;

  let add_contextual_lines ~sd lines =
    (* A contextual line is a line satisfying one of:
       + withing +/-1 lines of a multi-line label (top or bottom) ('multi line label contextual lines')
       + between two other rendered lines ('gap' contextual lines) *)
    let add_multi_line_label_contextual_lines =
      List.concat_map_with_next_and_prev
        ~f:(fun (l2 : Line.t with_line) ~prev:l1 ~next:l3 ->
          if List.is_empty l2.it.multi_line_labels
          then [ l2 ]
          else (
            let next_line = Line_index.(add l2.line 1) in
            let prefix =
              match l1 with
              | None when Line_index.(l2.line > initial) ->
                (* No preceeding line and [l2] isn't the first line *)
                [ line_of_idx ~sd Line_index.(sub l2.line 1) ]
              | Some l1 when Line_index.(diff l2.line l1.line) > 1 ->
                (* Preceeding line is not the immediately preceeding line to [l2] *)
                [ line_of_idx ~sd Line_index.(sub l2.line 1) ]
              | _ -> []
            in
            let postfix =
              match l3 with
              | None when Line_index.(next_line < (Source_reader.Line.last sd).idx) ->
                [ line_of_idx ~sd next_line ]
              | Some l3 when Line_index.(diff l3.line l2.line) > 1 ->
                [ line_of_idx ~sd next_line ]
              | _ -> []
            in
            prefix @ [ l2 ] @ postfix))
    in
    let add_gap_contextual_lines =
      List.concat_map_with_next ~f:(fun l1 ~next:l2 ->
        match l2 with
        | None -> [ l1 ]
        | Some l2 ->
          let line_delta = Line_index.(diff l2.line l1.line) in
          if line_delta <= 0
          then assert false
          else if line_delta = 1
          then [ l1 ]
          else if line_delta = 2
          then [ l1; line_of_idx ~sd Line_index.(add l1.line 1) ]
          else (* [line_delta > 2] *)
            [ l1 ])
    in
    lines |> add_multi_line_label_contextual_lines |> add_gap_contextual_lines
  ;;

  let group =
    let splitting_threshold = 2 in
    let block ~start ~rest = { start = start.line; lines = start.it :: rest } in
    let rec loop = function
      | [] -> assert false
      | [ l ] -> l, [], []
      | l1 :: ls ->
        let l2, ls, blocks = loop ls in
        if Line_index.(diff l2.line l1.line) >= splitting_threshold
        then l1, [], block ~start:l2 ~rest:ls :: blocks
        else l1, l2.it :: ls, blocks
    in
    function
    | [] -> []
    | ls ->
      let l, ls, blocks = loop ls in
      block ~start:l ~rest:ls :: blocks
  ;;

  module Line_labels = struct
    type t =
      { mutable inline_labels : Label.t list
      ; mutable multi_line_labels : Multi_line_label.t list
      }

    let create () = { inline_labels = []; multi_line_labels = [] }

    let add_inline_label t inline_label =
      t.inline_labels <- inline_label :: t.inline_labels
    ;;

    let add_multi_line_label t multi_line_label =
      t.multi_line_labels <- multi_line_label :: t.multi_line_labels
    ;;
  end

  let lines_of_labels ~sd labels =
    let range = range_of_labels labels in
    let enumerated_labels = List.mapi labels ~f:(fun i label -> i, label) in
    let _, rev_lines =
      Iter.fold
        (Source_reader.lines_in_range sd range)
        ~init:(enumerated_labels, [])
        ~f:(fun (labels, rev_lines) (line : Source_reader.Line.t) ->
          let line_start, line_stop = Source_reader.Line.split line in
          (* 1. Initialize per-line state *)
          let line_labels = Line_labels.create () in
          (* 2. Split labels into [inline_labels] and [multi_line_labels] and filter labels that end at this line *)
          let labels =
            List.filter labels ~f:(fun ((id, label) : _ * Label.t) ->
              let label_start, label_stop = Range.split label.range in
              if Byte_index.(line_start <= label_start && label_stop <= line_stop)
              then (
                (* Inline label *)
                Line_labels.add_inline_label line_labels label;
                false)
              else if Byte_index.(line_start <= label_start && label_start <= line_stop)
              then (
                (* Multi-line label that starts *)
                Line_labels.add_multi_line_label line_labels
                @@ Multi_line_label.Top
                     { id
                     ; start = Column_number.of_byte_index label_start ~line ~sd
                     ; priority = label.priority
                     };
                true)
              else if Byte_index.(label_start < line_start && line_stop < label_stop)
              then (* Multi-line label that goes through this line *)
                true
              else if Byte_index.(label_start < line_start && label_stop <= line_stop)
              then (
                (* Multi-line label that stops through this line *)
                Line_labels.add_multi_line_label line_labels
                @@ Multi_line_label.Bottom
                     { id
                     ; stop = Column_number.of_byte_index label_stop ~line ~sd
                     ; priority = label.priority
                     ; label = label.message
                     };
                false)
              else (* Label starts on a later line *)
                true)
          in
          (* Add to [rev_lines] *)
          ( labels
          , line_of_labels
              ~sd
              ~line
              line_labels.inline_labels
              line_labels.multi_line_labels
            :: rev_lines ))
    in
    List.rev rev_lines
  ;;

  let block_of_labels ~sd labels =
    labels |> lines_of_labels ~sd |> add_contextual_lines ~sd |> group
  ;;

  let of_diagnostic Diagnostic.{ severity; message; code; labels; notes } =
    let sources =
      labels
      |> group_labels_by_source
      |> List.map ~f:(fun (source, labels) ->
        let sd = Source_reader.open_source source in
        { source
        ; locus = locus_of_labels ~sd labels
        ; blocks = block_of_labels ~sd labels
        })
    in
    { severity; message; code; notes; sources = Rich sources }
  ;;
end

let of_diagnostic = Of_diagnostic.of_diagnostic

module Compact_of_diagnostic = struct
  let of_diagnostic Diagnostic.{ severity; message; code; labels; notes } =
    let sources =
      labels
      |> group_labels_by_source
      |> List.map ~f:(fun (source, labels) ->
        let sd = Source_reader.open_source source in
        let locus = locus_of_labels ~sd labels in
        source, locus)
    in
    { severity; message; code; notes; sources = Compact sources }
  ;;
end

let compact_of_diagnostic = Compact_of_diagnostic.of_diagnostic
