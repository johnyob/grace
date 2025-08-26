include Stdlib.Hashtbl

let find_or_add t key ~default =
  try find t key with
  | Not_found ->
    let value = default () in
    add t key value;
    value
;;
