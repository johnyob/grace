include Grace_std
include Grace
module Source_reader = Grace_source_reader
module Iter = IterLabels

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

  (** Shadow [Fmt.str_like] with a version that allows the caller to specify
      the margin for the produced string *)
  let str_like ?(margin = 78) like fmt =
    let buf = Buffer.create 64 in
    let bppf = Fmt.with_buffer ~like buf in
    Format.pp_set_geometry bppf ~max_indent:(margin - 1) ~margin;
    let flush ppf =
      Format.pp_print_flush ppf ();
      let s = Buffer.contents buf in
      Buffer.reset buf;
      s
    in
    Format.kfprintf flush bppf fmt
  ;;
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
