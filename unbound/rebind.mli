open Std_internal

type ('pat1, 'pat2) t = private 'pat1 * 'pat2 with sexp
val create : 'pat1 -> 'pat2 -> ('pat1, 'pat2) t
val type_rep : 'pat1 Type.Rep.t -> 'pat2 Type.Rep.t -> ('pat1, 'pat2) t Type.Rep.t

