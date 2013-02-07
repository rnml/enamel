open Core.Std

type 'v s =
| Ref of string (* free var *)
| Var of 'v    (* bound var *)
| Lam of ('v -> 'v s)
| App of 'v s * (string * 'v s) list
| Match_tuple of 'v s * int * ('v list -> 'v s)

val lam2 : ('v -> 'v -> 'v s) -> 'v s
val lam3 : ('v -> 'v -> 'v -> 'v s) -> 'v s
val lam4 : ('v -> 'v -> 'v -> 'v -> 'v s) -> 'v s

val app : 'v s * 'v s list -> 'v s

type t = { closed : 'a. unit -> 'a s } with sexp_of
