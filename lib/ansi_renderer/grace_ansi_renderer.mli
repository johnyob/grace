open Grace
module Config = Config

val pp_diagnostic
  :  ?config:Config.t
  -> ?code_to_string:('code -> string)
  -> unit
  -> 'code Diagnostic.t Fmt.t

val pp_compact_diagnostic
  :  ?config:Config.t
  -> ?code_to_string:('code -> string)
  -> unit
  -> 'code Diagnostic.t Fmt.t
