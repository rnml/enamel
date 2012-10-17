open Std_internal

type t

include Comparable.S with type t := t

val of_string : string -> t
val pretty : t -> Pretty.doc

