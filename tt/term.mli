open Std_internal

type t =
  | Typ of Level.t
  | Var of t Name.t
  | Con of Constant.t
  | Lam of (t s, t) Bind.t
  | App of t * t list
  | Fun of (t s, t) Bind.t

and 'a s = (* telescope *)
  | Nil
  | Cons of (t Name.t * 'a Embed.t, 'a s) Rebind.t

val type_rep   : t Type.Rep.t
val type_rep_of_s : 'a Type.Rep.t -> 'a s Type.Rep.t

module Name : Name.S with type a := t

val bind : (Name.t * t) list * t -> (t s, t) Bind.t
val unbind : (t s, t) Bind.t -> (Name.t * t) list * t
