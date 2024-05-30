include Core
include Grace
module Source_reader = Grace_source_reader
module Iter = IterLabels

module Format = struct
  include Format

  [%%if ocaml_version < (5, 2, 0)]

  let pp_max_margin = Int.max_value

  [%%else]

  let pp_max_margin = pp_infinity - 1

  [%%endif]
end

module Fmt = struct
  include Fmt

  let sp ppf () = Fmt.pf ppf " "
  let styled_multi style_multi t = List.fold_right style_multi ~init:t ~f:styled
  let with_style styles ppf t = styled_multi styles t ppf ()

  let repeat ~width t ppf x =
    for _ = 1 to width do
      t ppf x
    done
  ;;

  let sps n ppf x = repeat ~width:n sp ppf x
  let newline ppf () = Fmt.pf ppf "@."
end

module List = struct
  include List

  let concat_map_with_next t ~f =
    let rec loop = function
      | [] -> []
      | [ x ] -> f x ~next:None
      | x1 :: x2 :: t -> f x1 ~next:(Some x2) @ loop (x2 :: t)
    in
    loop t
  ;;

  let concat_map_with_next_and_prev t ~f =
    let rec loop t ~prev =
      match t with
      | [] -> []
      | [ x ] -> f x ~prev ~next:None
      | x1 :: x2 :: t ->
        let t' = f x1 ~prev ~next:(Some x2) in
        t' @ loop (x2 :: t) ~prev:(last t')
    in
    loop t ~prev:None
  ;;
end
