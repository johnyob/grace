open Core

(** The type of format strings associated with messages.

    + The input is always a [Format.formatter]
    + The result of [%a] and [%t] printing functions is [unit]. *)
type ('a, 'b) format = ('a, Format.formatter, unit, 'b) format4

module Severity : sig
  (** The type of severity.

      These are ordered in the following way:
      {[
        let () =
          let open Severity in
          assert (Bug > Error);
          assert (Error > Warning);
          assert (Warning > Note);
          assert (Note > Help)
        ;;
      ]} *)

  type t =
    | Help (** A help message. *)
    | Note (** A note. *)
    | Warning (** A warning. *)
    | Error (** An error. *)
    | Bug (** An unexpected bug. *)
  [@@deriving equal, compare, hash, sexp]

  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t

  val to_string : t -> string
end

module Priority : sig
  (** The type of priority. These are used to style the primary and secondary causes of a diagnostic.

      These are ordered in the following way:
      {[
        let () = assert (Priority.(Primary > Secondary))
      ]} *)

  type t =
    | Secondary
    (** Priority to describe labels that explain the secondary causes of a diagnostic. *)
    | Primary
    (** Priority to describe labels that explain the primary cause of a diagnostic. *)
  [@@deriving equal, compare, hash, sexp]

  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t

  val is_primary : t -> bool
  val is_secondary : t -> bool
  val to_string : t -> string
end

module Message : sig
  (** The type of messages.

      Messages are unrendered formatted strings. The rendering is delayed till Grace's renderering engine
      since layout decisions are it's responsibility.

      A valid message must satisfy the following two conditions:
      + Messages must be encoded using ASCII.
      + Messages must not contain control characters such as the newline character [\n].

      Equality and comparison of messages is performed on the hash of the messages rendered
      contents. *)
  type t = Format.formatter -> unit [@@deriving equal, hash, sexp]

  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t

  (** [create str] converts the string [str] into a message. *)
  val create : string -> t

  (** [createf fmt ...] formats a message. *)
  val createf : ('a, t) format -> 'a

  (** [kcreatef kont fmt ...] is equivalent to [kont (createf fmt ...)]. *)
  val kcreatef : (t -> 'b) -> ('a, 'b) format -> 'a

  (** converts a {{!type:t} message} into a string by formatting it with the maximum admissible margin. *)
  val to_string : t -> string

  (** alias of {{!val:create} [Message.create]}. *)
  val of_string : string -> t
end

module Label : sig
  (** The type of labels.

      Labels describe an underlined region of code associated with a diagnostic. *)
  type t =
    { range : Range.t (** The range we are going to include in the rendered diagnostic. *)
    ; priority : Priority.t (** The priority (or style) of the label. *)
    ; message : Message.t
      (** A message providing additional information for the underlined code. *)
    }
  [@@deriving equal, hash, sexp]

  (** [create ~range ~priority message] constructs a label.

      @param range the range to underline.
      @param priority the priority of the label. *)
  val create : range:Range.t -> priority:Priority.t -> Message.t -> t

  (** [createf ~range ~priority fmt ...] constructs a label with a formatted message.

      @param range the range to underline.
      @param priority the priority of the label. *)
  val createf : range:Range.t -> priority:Priority.t -> ('a, t) format -> 'a

  (** [kcreatef ~range ~priority kont fmt ...] is equivalent to [kont (createf ~range ~priority fmt ...)].

      @param range the range to underline.
      @param priority the priority of the label. *)
  val kcreatef
    :  range:Range.t
    -> priority:Priority.t
    -> (t -> 'b)
    -> ('a, 'b) format
    -> 'a

  (** [primary ~range message] is equivalent to [create ~range ~priority:Primary message]. *)
  val primary : range:Range.t -> Message.t -> t

  (** [primaryf ~range fmt ...] is equivalent to [createf ~range ~priority:Primary fmt ...]. *)
  val primaryf : range:Range.t -> ('a, t) format -> 'a

  (** [kprimaryf ~range kont fmt ...] is equivalent to [kcreatef ~range ~priority:Primary kont fmt ...]. *)
  val kprimaryf : range:Range.t -> (t -> 'b) -> ('a, 'b) format -> 'a

  (** [secondary ~range message] is equivalent to [create ~range ~priority:Secondary message]. *)
  val secondary : range:Range.t -> Message.t -> t

  (** [secondaryf ~range fmt ...] is equivalent to [createf ~range ~priority:Secondary fmt ...]. *)
  val secondaryf : range:Range.t -> ('a, t) format -> 'a

  (** [ksecondaryf ~range kont fmt ...] is equivalent to [kcreatef ~range ~priority:Secondary kont fmt ...]. *)
  val ksecondaryf : range:Range.t -> (t -> 'b) -> ('a, 'b) format -> 'a
end

(** The type of diagnostics. *)
type 'code t =
  { severity : Severity.t (** The overall severity of the diagnostic. *)
  ; message : Message.t
    (** The main message associated with the diagnostic. These should not include control characters (such as the newline character [\n]).
      To support compact rendering, the message should be specific enough to make sense on its own, without the additional context provided
      by labels and notes. *)
  ; code : 'code option (** The (optional) error code assicoated with the diagnostic *)
  ; labels : Label.t list
    (** Labels that describe the cause of the diagnostic. The order of the labels has no meaning,
      Grace's rendering engine will determine the order they appear. *)
  ; notes : Message.t list
    (** Notes that are associated with the primary cause of the diagnostic. *)
  }
[@@deriving sexp]

(** [create severity message] constructs a diagnostic with the [message].

    @param notes additional notes associated with the primary cause of the diagnostic.
    @param labels used to describe the cause of the diagnostic.
    @param code the error code of the diagnostic. *)
val create
  :  ?notes:Message.t list
  -> ?labels:Label.t list
  -> ?code:'code
  -> Severity.t
  -> Message.t
  -> 'code t

(** [createf severity fmt ...] formats a message and constructs a diagnostic.

    @param notes additional notes associated with the primary cause of the diagnostic.
    @param labels used to describe the cause of the diagnostic.
    @param code the error code of the diagnostic. *)
val createf
  :  ?notes:Message.t list
  -> ?labels:Label.t list
  -> ?code:'code
  -> Severity.t
  -> ('a, 'code t) format
  -> 'a

(** [kcreatef kont severity fmt ...] is equivalent to [kont (createf severity fmt ...)].

    @param notes additional notes associated with the primary cause of the diagnostic.
    @param labels used to describe the cause of the diagnostic. *)
val kcreatef
  :  ?notes:Message.t list
  -> ?labels:Label.t list
  -> ?code:'code
  -> ('code t -> 'b)
  -> Severity.t
  -> ('a, 'b) format
  -> 'a
