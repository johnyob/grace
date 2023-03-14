(* FIXME: Do about, etc *)
(** A diagnostic is a message with associated debugging context for the user, for example a compiler error. *)

include module type of Index (** @inline *)

(** Source code ranges, also known as 'locations'. *)
module Range = Range

(** Source is a file-like abstraction. *)
module Source = Source

(** Diagnostic types and constructors. *)
module Diagnostic = Diagnostic
