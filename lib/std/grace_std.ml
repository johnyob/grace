module Array = ArrayLabels
module Comparable = Comparable
module Format = Format
module Fmt = Fmt
module List = ListLabels
module Pretty_printer = Pretty_printer
module String = String

let invalid_argf fmt = Format.kasprintf invalid_arg fmt
