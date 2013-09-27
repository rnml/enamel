open Std_internal

module Term : sig

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
  val s_type_rep : 'a Type.Rep.t -> 'a s Type.Rep.t

  module Name : Name.S with type a := t

  val bind : (Name.t * t) list * t -> (t s, t) Bind.t
  val unbind : (t s, t) Bind.t -> (Name.t * t) list * t

end

module Inductive_type : sig

  type arg =
    | Rec of Term.t list
    | Nonrec of Term.t

  type body = {
    tycon : Constant.t;
    kind : (Term.t Term.s, Level.t) Bind.t;
    constructors : ((Term.t, arg) Bind.t Term.s, Term.t list) Bind.t Constant.Map.t;
  }

  type t = (Term.t Term.s, body) Bind.t

  (*
     data T (Gamma) : (Delta) -> Type =
     | ...
     | C (Gamma) : (Theta) -> T (Gamma) (Row)
     | ...

  *)

end
