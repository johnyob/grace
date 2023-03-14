open! Import
open Core_unix

(** A file is abstracted as a (memory-mapped) bigstring *)
type file =
  { descr : File_descr.t
  ; content : (Bigstring.t[@sexp.opaque]) [@equal.ignore] [@compare.ignore] [@hash.ignore]
  }
[@@deriving equal, compare, hash, sexp]

module File_table : sig
  (** The abstract type of the file table *)
  type t

  val create : unit -> t

  (** [open_file tbl fname] opens the file with filename [fname] *)
  val open_file : t -> string -> file

  (** [close_all_files tbl] closes all files, clearing the file table. *)
  val close_all_files : t -> unit
end = struct
  type t = (string, file) Hashtbl.t

  let create () : t = Hashtbl.create (module String)

  let open_file t fname =
    Hashtbl.find_or_add t fname ~default:(fun () ->
      let fd = openfile ~mode:[ O_RDONLY ] fname in
      let content =
        try
          let size = -1 in
          Bigstring_unix.map_file ~shared:false fd size
        with
        | _ ->
          close fd;
          sys_errorf "could not read the file %s" fname
      in
      { descr = fd; content })
  ;;

  let close_all_files t =
    Hashtbl.iter t ~f:(fun file -> close file.descr);
    Hashtbl.clear t
  ;;
end

module Source_descr = struct
  module T = struct
    type content =
      | File of file
      | String of string
      | Reader of Source.reader
    [@@deriving equal, compare, hash, sexp]

    type t =
      { content : content
      ; source : Source.t
      }
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)

  let[@inline] source t = t.source
end

let length (sd : Source_descr.t) =
  match sd.content with
  | File file -> Bigstring.length file.content
  | String string -> String.length string
  | Reader reader -> reader.length
;;

let unsafe_get (sd : Source_descr.t) i =
  match sd.content with
  | File file -> Bigstring.unsafe_get file.content i
  | String str -> String.unsafe_get str i
  | Reader reader -> reader.unsafe_get i
;;

let slice sd range =
  if not (Source.equal (Source_descr.source sd) (Range.source range))
  then invalid_argf "mismatching source";
  let start, stop = Range.split range in
  let buf = Buffer.create (Byte_index.diff stop start) in
  for i = (start :> int) to (stop :> int) - 1 do
    Buffer.add_char buf (unsafe_get sd i)
  done;
  Buffer.contents buf
;;

let slicei sd (start : Byte_index.t) (stop : Byte_index.t) =
  let buf = Buffer.create (Byte_index.diff stop start) in
  for i = (start :> int) to (stop :> int) - 1 do
    Buffer.add_char buf (unsafe_get sd i)
  done;
  Buffer.contents buf
;;

module Line_starts = struct
  (** The type of line starts.

      For computation of line numbers from ranges, we require a function mapping {{!type:Line_index.t} line indices} to {{!type:Byte_index.t} byte indicies}
      and the maximum line index.

      A valid [line_starts] for a [source] satisfies:
      + ... *)
  type t =
    { unsafe_get : int -> Byte_index.t
    ; length : int
    }

  type fn = Source_descr.t -> t

  let default_fn sd =
    let line_starts_array =
      (* Prefix with zero since no preceeding newline *)
      let line_starts = ref [ Byte_index.initial ] in
      for idx = 0 to length sd - 1 do
        match unsafe_get sd idx with
        | '\n' -> line_starts := Byte_index.of_int (idx + 1) :: !line_starts
        | _ -> ()
      done;
      Array.of_list @@ List.rev !line_starts
    in
    { unsafe_get = (fun idx -> Array.unsafe_get line_starts_array (idx :> int))
    ; length = Array.length line_starts_array
    }
  ;;

  let[@inline] length t = t.length
  let[@inline] unsafe_get t idx = t.unsafe_get idx

  let find t (idx : Byte_index.t) =
    (* Safety: t.line_starts is known to be non-empty, hence `Last_less_than_or_equal_to
       will always return an index *)
    Binary_search.binary_search
      t
      ~length
      ~get:unsafe_get
      ~compare:Byte_index.compare
      `Last_less_than_or_equal_to
      idx
    |> Option.value_exn ~here:[%here]
  ;;
end

type error =
  [ `Already_initialized
  | `Not_initialized
  ]

exception Error of error

type t =
  { line_starts_table : Line_starts.t Source_descr.Table.t
  ; file_table : File_table.t
  ; line_starts_fn : Line_starts.fn
  }

let state : t option ref = ref None

let get () =
  match !state with
  | Some t -> t
  | None -> raise @@ Error `Not_initialized
;;

let init ?(line_starts_fn = Line_starts.default_fn) () =
  if Option.is_some !state then raise @@ Error `Already_initialized;
  state
  := Some
       { line_starts_fn
       ; file_table = File_table.create ()
       ; line_starts_table = Source_descr.Table.create ()
       }
;;

let clear () =
  match !state with
  | None -> ()
  | Some { file_table; _ } ->
    File_table.close_all_files file_table;
    state := None
;;

let with_reader ?line_starts_fn f =
  Fun.protect
    (fun () ->
      init ?line_starts_fn ();
      f ())
    ~finally:clear
;;

let line_starts sd =
  let t = get () in
  Hashtbl.find_or_add t.line_starts_table sd ~default:(fun () -> t.line_starts_fn sd)
;;

let open_source (source : Source.t) : Source_descr.t =
  let content =
    match source with
    | `File fname ->
      let file = File_table.open_file (get ()).file_table fname in
      Source_descr.File file
    | `String { content; _ } -> String content
    | `Reader reader -> Reader reader
  in
  { source; content }
;;

module Line = struct
  type t =
    { idx : Line_index.t
    ; range : Range.t
    }
  [@@deriving sexp]

  let[@inline] start t = Range.start t.range
  let[@inline] stop t = Range.stop t.range
  let[@inline] split t = Range.split t.range

  let last sd =
    let idx = Line_index.of_int @@ Line_starts.length (line_starts sd) in
    let length = Byte_index.of_int @@ length sd in
    { idx; range = Range.create ~source:sd.source length length }
  ;;

  let offset sd (idx : Line_index.t) : Byte_index.t =
    let line_starts = line_starts sd in
    let last_line_index = Line_index.of_int @@ Line_starts.length line_starts in
    if Line_index.(initial <= idx && idx < last_line_index)
    then Line_starts.unsafe_get line_starts (idx :> int)
    else if Line_index.(idx = last_line_index)
    then Byte_index.of_int @@ length sd
    else
      invalid_argf
        "%a > %a: line index exceeds the last line index"
        Line_index.pp
        idx
        Line_index.pp
        last_line_index
  ;;

  let of_line_index sd (idx : Line_index.t) : t =
    let start = offset sd idx
    and stop = offset sd Line_index.(add idx 1) in
    { idx; range = Range.create ~source:sd.source start stop }
  ;;

  let of_byte_index sd (idx : Byte_index.t) : t =
    let line_starts = line_starts sd in
    Line_starts.find line_starts idx |> Line_index.of_int |> of_line_index sd
  ;;

  let[@inline] slice t ~sd = slice sd t.range
end

let lines sd : Line.t Iter.t =
  fun f ->
  let line_starts = line_starts sd in
  let stop = Line_starts.length line_starts in
  for line_idx = 0 to stop - 1 do
    f (Line.of_line_index sd @@ Line_index.of_int line_idx)
  done
;;

let lines_in_range sd range : Line.t Iter.t =
  fun f ->
  if not (Source.equal (Source_descr.source sd) (Range.source range))
  then invalid_argf "mismatching sources";
  let line_starts = line_starts sd in
  let start, stop = Range.split range in
  for
    line_idx = Line_starts.find line_starts start to Line_starts.find line_starts stop
  do
    f (Line.of_line_index sd @@ Line_index.of_int line_idx)
  done
;;
