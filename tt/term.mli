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
with sexp_of

val type_rep   : t Type.Rep.t
val type_rep_of_s : 'a Type.Rep.t -> 'a s Type.Rep.t

module Name : Name.S with type a := t

module Binds : sig

  type ('a, 'b) t = ('a s, 'b) Bind.t

  type ('a, 'b) e = (Name.t * 'a) list * 'b

  val type_rep : 'a Type.Rep.t -> 'b Type.Rep.t -> ('a, 'b) t Type.Rep.t

  val unbind : 'a Type.Rep.t -> 'b Type.Rep.t -> ('a, 'b) t -> ('a, 'b) e

  val bind : ('a, 'b) e -> ('a, 'b) t

  val map
    :  ('a1, 'b1) t
    -> args:('a1 Type.Rep.t * ((Name.t * 'a1) list -> (Name.t * 'a2) list))
    -> body:('b1 Type.Rep.t * ('b1 -> 'b2))
    -> ('a2, 'b2) t

end

val bind   : (Name.t * t) list * t -> (t s, t) Bind.t
val unbind : (t s, t) Bind.t -> (Name.t * t) list * t

val unbind_s : 'a s -> (Name.t * 'a) list
val bind_s   : (Name.t * 'a) list -> 'a s

val pretty   : t -> Pretty.t
val pretty_s : ('a -> Pretty.t) -> 'a s -> Pretty.t

val concat : 'a s list -> 'a s
