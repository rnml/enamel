open Std_internal

type 'pat t = private 'pat with sexp

val create : 'pat -> 'pat t

val type_rep : 'pat Type.Rep.t -> 'pat t Type.Rep.t

