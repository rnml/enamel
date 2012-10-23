open Core.Std

type t

val empty : t
val add : t -> string -> t
val concat : t -> t -> t
val dump : t -> string

