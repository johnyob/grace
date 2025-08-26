(* Added in OCaml 5.1 *)
let[@warning "-32"] is_empty t =
  match t with
  | [] -> true
  | _ :: _ -> false
;;

include Stdlib.StdLabels.List

let pp ?pp_sep pp_elt = Format.pp_print_list ?pp_sep pp_elt
let sexp_of_t = Sexplib.Std.sexp_of_list
let t_of_sexp = Sexplib.Std.list_of_sexp
let sort t ~compare = sort t ~cmp:compare
let stable_sort t ~compare = stable_sort t ~cmp:compare
let hd_exn = hd

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let rec last t =
  match t with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

let count t ~f = fold_left t ~init:0 ~f:(fun acc x -> if f x then acc + 1 else acc)

let max_elt t ~compare =
  match t with
  | [] -> None
  | elt :: t ->
    Some
      (fold_left t ~init:elt ~f:(fun acc elt -> if compare acc elt < 0 then elt else acc))
;;

let group t ~break =
  let t = ref t in
  let[@tail_mod_cons] rec take_group () =
    match !t with
    | ([] | [ _ ]) as group ->
      t := [];
      group
    | x :: (y :: _ as tl) ->
      t := tl;
      if break x y then [ x ] else x :: take_group ()
  in
  let[@tail_mod_cons] rec groups () =
    if is_empty !t
    then []
    else (
      let group = take_group () in
      group :: groups ())
  in
  groups ()
;;

let sort_and_group t ~compare =
  t |> stable_sort ~compare |> group ~break:(fun x y -> compare x y <> 0)
;;
