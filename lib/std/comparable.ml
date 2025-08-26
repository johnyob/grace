module type S = sig
  type t

  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end

module Make (T : sig
    type t

    val compare : t -> t -> int
  end) : S with type t := T.t = struct
  include T

  let equal x y = compare x y = 0
  let ( >= ) x y = compare x y >= 0
  let ( <= ) x y = compare x y <= 0
  let ( > ) x y = compare x y > 0
  let ( < ) x y = compare x y < 0
  let ( = ) x y = equal x y
  let ( <> ) x y = compare x y <> 0
  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
end
