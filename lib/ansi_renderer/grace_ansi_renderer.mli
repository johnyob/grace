open Grace
module Config = Config

type ('code, 'r) with_params :=
  ?config:Config.t -> ?code_to_string:('code -> string) -> 'r

val pp_diagnostic : ('code, 'code Diagnostic.t Fmt.t) with_params
val pp_compact_diagnostic : ('code, 'code Diagnostic.t Fmt.t) with_params
val output_diagnostic : ('code, Out_channel.t -> 'code Diagnostic.t -> unit) with_params

val output_compact_diagnostic
  : ('code, Out_channel.t -> 'code Diagnostic.t -> unit) with_params

val pr_diagnostic : ('code, 'code Diagnostic.t -> unit) with_params
val pr_compact_diagnostic : ('code, 'code Diagnostic.t -> unit) with_params
val epr_diagnostic : ('code, 'code Diagnostic.t -> unit) with_params
val epr_compact_diagnostic : ('code, 'code Diagnostic.t -> unit) with_params
