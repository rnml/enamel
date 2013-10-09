open Std_internal

type t with sexp_of

val create : unit -> t

val assert_leq : t -> t -> [ `Ok | `Inconsistent ]
val assert_lt  : t -> t -> [ `Ok | `Inconsistent ]

val type_rep : t Type.Rep.t
