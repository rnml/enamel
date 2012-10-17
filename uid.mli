open Std_internal

type t

include Comparable.S with type t := t

val create : unit -> t
