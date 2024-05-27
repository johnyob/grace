open Grace
module Config = Config

module Ansi : sig
  val pp_diagnostic : config:Config.t -> Diagnostic.t Fmt.t
end
