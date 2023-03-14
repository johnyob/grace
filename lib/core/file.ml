open Core
open Source

module Id : sig
  type t

  include Identifiable.S with type t := t

  val create : unit -> t
end = struct
  include Int

  let create =
    let next = ref 0 in
    fun () ->
      Int.incr next;
      !next
  ;;
end

type t =
  { name : string
  ; source : string (* TODO: Bigstring *)
  ; line_starts : Byte_index.t Array.t
  }

let create name source =
  let line_starts =
    (* Prefix with zero since no preceeding newline *)
    0
    :: (source
       |> String.to_list
       |> List.filter_mapi ~f:(fun idx c ->
              match c with
              | '\n' -> Some (Byte_index.create (idx + 1))
              | _ -> None))
  in
  { name; source; line_starts = Array.of_list line_starts }
;;

let name t = t.name
let source t = t.source
let stop t = String.length t.source

module Source = struct
  let range t = Range.create Byte_index.initial (stop t)

  let slice t ({ start; stop } : Range.t) =
    String.sub t.source ~pos:start ~len:(stop - start)
  ;;
end

module Line = struct
  let last t : Line_index.t = Line_index.create (Array.length t.line_starts)

  let start t (idx : Line_index.t) : Byte_index.t option =
    let last_line_index = last t in
    if Line_index.(initial <= idx && idx < last_line_index)
    then Some (Array.unsafe_get t.line_starts (idx :> int))
    else if Line_index.(idx = last_line_index)
    then Some (stop t)
    else None
  ;;

  let starts t : Byte_index.t Array.t = t.line_starts

  let range t (idx : Line_index.t) : Range.t option =
    let open Option.Let_syntax in
    let%map curr_line_start = start t idx
    and next_line_start = start t Line_index.(idx + 1) in
    Range.create curr_line_start next_line_start
  ;;

  let index t (idx : Byte_index.t) : Line_index.t =
    Binary_search.binary_search
      t.line_starts
      ~length:Array.length
      ~get:Array.get
      ~compare:Byte_index.compare
      `Last_less_than_or_equal_to
      idx
    |> Option.value_exn ~here:[%here]
  ;;

  let slice t idx = Option.(range t idx >>| Source.slice t)
end

module Line_range = struct
  type t =
    { line : Line_index.t
    ; range : Range.Column_index.t
    }
  [@@deriving sexp]

  let start file range =
    let start, stop = Range.(start range, stop range) in
    let line = Line.index file start in
    (* [Line.index] always returns a valid index into [file.line_starts] *)
    let line_start = Array.unsafe_get file.line_starts line in
    let next_line_start = Line.start file Line_index.(line + 1) in
    { line
    ; range =
        Range.Column_index.create
          (start - line_start)
          (Option.value next_line_start ~default:stop - line_start)
    }
  ;;

  let next file range curr =
    let next_line = Line_index.(curr.line + 1) in
    match Line.start file next_line with
    | None -> None
    | Some next_line_start ->
      if Range.contains range next_line_start
      then (
        let start = file.line_starts.(curr.line) + Range.Column_index.(stop curr.range) in
        let stop = Range.stop range in
        let next_next_line_start = Line.start file Line_index.(next_line + 1) in
        Some
          { line = next_line
          ; range =
              Range.Column_index.create
                (start - next_line_start)
                (Option.value next_next_line_start ~default:stop - next_line_start)
          })
      else None
  ;;

  let slice file { line; range } =
    let line_start = file.line_starts.(line) in
    let start, stop = Range.Column_index.(start range, stop range) in
    Source.slice file (Range.create (line_start + start) (line_start + stop))
  ;;

  let all file range =
    let init = start file range in
    let rec loop curr all =
      let next = next file range curr in
      match next with
      | None -> List.rev all
      | Some next -> loop next (curr :: all)
    in
    loop init []
  ;;
end