module type S = sig
  type t

  val pp : t Fmt.t
end
