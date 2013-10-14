open Std_internal

type arg =
  | Rec of (Term.t, Term.t list) Term.Binds.t
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t, Level.t) Term.Binds.t;
  cons : (arg, Term.t list) Term.Binds.t Constant.Map.t;
}

type t = (Term.t, body) Term.Binds.t

val type_rep_of_arg  : arg  Type.Rep.t
val type_rep_of_body : body Type.Rep.t
val type_rep         : t    Type.Rep.t

val kind : t -> Term.t
val cons : t -> Term.t Constant.Map.t
val elim : t -> Term.t

val pretty : t -> Pretty.t
