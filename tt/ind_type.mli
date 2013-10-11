open Std_internal

module Binds : sig

  type ('a, 'b) t = ('a Term.s, 'b) Bind.t

  type ('a, 'b) e = (Term.Name.t * 'a) list * 'b

  val type_rep : 'a Type.Rep.t -> 'b Type.Rep.t -> ('a, 'b) t Type.Rep.t

  val unbind : 'a Type.Rep.t -> 'b Type.Rep.t -> ('a, 'b) t -> ('a, 'b) e

  val bind : ('a, 'b) e -> ('a, 'b) t

end

type arg =
  | Rec of Term.t list
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t, Level.t) Binds.t;
  cons : ((Term.t, arg) Binds.t, Term.t list) Binds.t Constant.Map.t;
}

type t = (Term.t Term.s, body) Bind.t

val type_rep_of_arg  : arg  Type.Rep.t
val type_rep_of_body : body Type.Rep.t
val type_rep         : t    Type.Rep.t

val kind : t -> Term.t
val cons : t -> Term.t Constant.Map.t
val elim : t -> Term.t
