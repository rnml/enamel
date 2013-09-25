open Std_internal

module Term : sig

  type t =
    | Typ of int
    | Var of t Name.t
    | Con of Constant.t
    | Lam of (s, t) Bind.t
    | App of t * t list
    | Fun of (s, t) Bind.t

  and s = (* telescope *)
    | Nil
    | Cons of (t Name.t * t Embed.t, s) Rebind.t

  val type_rep   : t Type.Rep.t
  val s_type_rep : s Type.Rep.t

  module Name : Name.S with type a := t

  val bind : (Name.t * t) list * t -> (s, t) Bind.t
  val unbind : (s, t) Bind.t -> (Name.t * t) list * t

end
