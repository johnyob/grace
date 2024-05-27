open Grace
module Config = Config

val pp_diagnostic : config:Config.t -> Diagnostic.t Fmt.t
