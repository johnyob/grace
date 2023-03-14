open Grace
module Config = Config

val ppd_rich : config:Config.t -> files:Files.t -> Diagnostic.t -> Fmt_doc.t
val ppd_fizzbuzz : Fmt_doc.t
