open Std_internal

type t with sexp

include Comparable.S with type t := t

val of_string : string -> t
val pretty : t -> Pretty.doc

