open Core
open File

type t = (File.Id.t, File.t) Hashtbl.t

let create () = Hashtbl.create (module File.Id)

let add t name source =
  let file_id = File.Id.create () in
  Hashtbl.set t ~key:file_id ~data:(File.create name source);
  file_id
;;

let find t file_id = Hashtbl.find_exn t file_id
let name t file_id = (find t file_id).name

module Source = struct
  let range t file_id = Source.range (find t file_id)
  let slice t file_id range = Source.slice (find t file_id) range
end

module Line = struct
  let starts t file_id = Line.starts (find t file_id)
  let last t file_id = Line.last (find t file_id)
  let start t file_id idx = Line.start (find t file_id) idx
  let range t file_id idx = Line.range (find t file_id) idx
  let index t file_id idx = Line.index (find t file_id) idx
  let slice t file_id idx = Line.slice (find t file_id) idx
end

module Line_range = struct
  let start t file_id range = Line_range.start (find t file_id) range
  let next t file_id range curr = Line_range.next (find t file_id) range curr
  let all t file_id range = Line_range.all (find t file_id) range
  let slice t file_id line_range = Line_range.slice (find t file_id) line_range
end
