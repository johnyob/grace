open Grace
module Config = Config

(* temp *)
module Snippet = Snippet

val ppd_rich : config:Config.t -> files:File.Cache.t -> Diagnostic.t -> Fmt_doc.t
val ppd_simple : config:Config.t -> files:File.Cache.t -> Diagnostic.t -> Fmt_doc.t
val ppd_fizzbuzz : Fmt_doc.t
