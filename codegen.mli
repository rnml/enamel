open Core.Std

type sexp = Sexp.t = Atom of string | List of sexp list

module Void : sig
  type t
  val absurd : t -> _
end

module Quasi_quotation : sig

  type 'v s =
  | Var of 'v
  | Lam of ('v -> 'v s)
  | App of 'v s * 'v s list

  val lam2 : ('v -> 'v -> 'v s) -> 'v s
  val lam3 : ('v -> 'v -> 'v -> 'v s) -> 'v s
  val lam4 : ('v -> 'v -> 'v -> 'v -> 'v s) -> 'v s

  type t = { closed : 'a. unit -> 'a s } with sexp_of

end
