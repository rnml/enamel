open Std_internal

type arg =
  | Rec of Term.t list
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t Term.s, Level.t) Bind.t;
  tmcons :
    ((Term.t, arg) Bind.t Term.s, Term.t list) Bind.t Constant.Map.t;
}

type t = (Term.t Term.s, body) Bind.t

val type_rep_of_arg  : arg  Type.Rep.t
val type_rep_of_body : body Type.Rep.t
val type_rep         : t    Type.Rep.t

