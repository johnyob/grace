module type S = sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t
end
