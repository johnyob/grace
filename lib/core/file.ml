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
    source
    |> String.to_list
    |> List.filter_mapi ~f:(fun idx c ->
           match c with
           | '\n' -> Some (Byte_index.create idx)
           | _ -> None)
    |> Array.of_list
  in
  { name; source; line_starts }
;;

let name t = t.name
let last_line_index t : Line_index.t = Line_index.create (Array.length t.line_starts)

let line_start t (idx : Line_index.t) : Byte_index.t =
  let last_line_index = last_line_index t in
  if Line_index.(initial <= idx && idx < last_line_index)
  then Array.unsafe_get t.line_starts (idx :> int)
  else
    raise_s
      [%message
        "Line index out of bounds" (idx : Line_index.t) (last_line_index : Line_index.t)]
;;

let line_range t (idx : Line_index.t) =
  let line_start = line_start t idx in
  let next_line_start = Line_index.(line_start + 1) in
  Range.create line_start next_line_start
;;

let line_index t (idx : Byte_index.t) : Line_index.t =
  Binary_search.binary_search
    t.line_starts
    ~length:Array.length
    ~get:Array.get
    ~compare:Byte_index.compare
    `First_greater_than_or_equal_to
    idx
  |> Option.value_exn
       ~here:[%here]
       ~error:(Error.create_s [%message "Byte index out of bounds" (idx : Byte_index.t)])
;;

let location t (idx : Byte_index.t) : Text.Location.t =
  let line_index = line_index t idx in
  let line_start_index = line_start t line_index in
  let line_src =
    String.sub t.source ~pos:line_start_index ~len:(idx - line_start_index)
  in
  let column = Column_index.create (String.length line_src) in
  Text.(
    Location.
      { line = Line_number.of_index line_index
      ; column = Column_number.of_index column ~line:line_src
      })
;;

let line_number t (idx : Byte_index.t) : Text.Line_number.t =
  let line_idx = line_index t idx in
  Text.Line_number.of_index line_idx
;;

let line_span t (idx : Line_index.t) : Text.Span.t =
  let open Text in
  let line_start = line_start t idx in
  let next_line_start = Line_index.(line_start + 1) in
  Span.create
    Location.{ line = Line_number.of_index line_start; column = Column_number.initial }
    Location.
      { line = Line_number.of_index next_line_start; column = Column_number.initial }
;;

let source t = t.source
let source_range t = Range.of_string t.source

let source_slice t ({ start; stop } : Range.t) =
  String.sub t.source ~pos:start ~len:(stop - start)
;;

module Cache = struct
  let create_file = create

  type nonrec t = (Id.t, t) Hashtbl.t

  let create () = Hashtbl.create (module Id)

  let add t name source =
    let file_id = Id.create () in
    Hashtbl.set t ~key:file_id ~data:(create_file name source);
    file_id
  ;;

  let find t file_id = Hashtbl.find_exn t file_id
  let name t file_id = (find t file_id).name
  let line_start t file_id idx = line_start (find t file_id) idx

  let line_range t file_id (idx : Line_index.t) : Range.t =
    line_range (find t file_id) idx
  ;;

  let line_index t file_id idx = line_index (find t file_id) idx
  let source t file_id = source (find t file_id)
  let source_range t file_id = source_range (find t file_id)
  let source_slice t file_id range = source_slice (find t file_id) range
  let location t file_id idx = location (find t file_id) idx
  let line_number t file_id idx = line_number (find t file_id) idx
  let line_span t file_id idx = line_span (find t file_id) idx
end
