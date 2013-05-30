open Core.Std

val to_sexp : 'a Type.Rep.t -> 'a -> Sexp.t
val of_sexp : 'a Type.Rep.t -> Sexp.t -> 'a
