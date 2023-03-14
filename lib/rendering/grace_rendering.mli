open Grace
module Config = Config

val ppd_rich : ?debug:bool -> config:Config.t -> files:Files.t -> Diagnostic.t -> Fmt_doc.t
val ppd_compact : config:Config.t -> files:Files.t -> Diagnostic.t -> Fmt_doc.t
