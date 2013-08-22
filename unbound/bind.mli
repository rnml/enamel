open Std_internal

type ('pat, 'term) t with sexp
val create : 'pat -> 'term -> ('pat, 'term) t
val unbind : 'pat Type.Rep.t -> 'term Type.Rep.t -> ('pat, 'term) t -> 'pat * 'term
val type_rep : 'pat Type.Rep.t -> 'term Type.Rep.t -> ('pat, 'term) t Type.Rep.t

