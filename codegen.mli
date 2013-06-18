open Core.Std
open Core_extended.Std

module Type : sig
  type t =
  | Int
  | Char
  | Float
  | String
  | Bool
  | Unit
  | Option of t
  | List of t
  | Array of t
  | Lazy of t
  | Ref of t
  | Pair of t * t
  | Triple of t * t * t
  | Record of (string * t) list
  | Variant of (string * t) list
  | Name of string
  with sexp

  val render : t -> Text_block.t
end

module rec Term : sig
  type 'v s =
  | Ref of string (* free var *)
  | Var of 'v     (* bound var *)
  | Lam of ('v -> 'v s)
  | App of 'v s * (string option * 'v s) list
  | Match_tuple of 'v s * int * ('v list -> 'v s)
  | Struct of 'v Decl.s list
  | Sig    of 'v Decl.s list
  | Module of 'v Decl.s list * string

  val lam2 : ('v -> 'v -> 'v s) -> 'v s
  val lam3 : ('v -> 'v -> 'v -> 'v s) -> 'v s
  val lam4 : ('v -> 'v -> 'v -> 'v -> 'v s) -> 'v s

  val app : 'v s * 'v s list -> 'v s

  type t = { closed : 'v. unit -> 'v s } with sexp_of

end

and Decl : sig
  type kind = Value | Type | Module | Signature
  type 'v s = kind * string * 'v Term.s option

  type t = { closed : 'v. unit -> 'v s } with sexp_of

end

