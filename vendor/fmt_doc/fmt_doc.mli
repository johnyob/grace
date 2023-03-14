type t

val render : Format.formatter -> t -> unit
val empty : t
val repeat : int -> t -> t
val newline : t
val sp : t
val cut : t
val comma : t
val semi : t
val append : t -> t -> t
val ( ++ ) : t -> t -> t
val concat : ?sep:t -> t list -> t
val box : ?indent:int -> t -> t
val hbox : t -> t
val vbox : ?indent:int -> t -> t
val hvbox : ?indent:int -> t -> t
val hovbox : ?indent:int -> t -> t
val char : char -> t
val bool : bool -> t
val int : int -> t
val string : string -> t
val styled : Fmt.style -> t -> t
val of_pp : 'a Fmt.t -> 'a -> t
val option : ?none:t -> ('a -> t) -> 'a option -> t