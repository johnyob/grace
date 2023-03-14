open Grace
module Config = Config
module Source_reader = Source_reader

module Ansi : sig
  val pp_diagnostic : config:Config.t -> Diagnostic.t Fmt.t
end
