open Std_internal

type 'term t = private 'term with sexp

val create : 'term -> 'term t

val type_rep : 'term Type.Rep.t -> 'term t Type.Rep.t
