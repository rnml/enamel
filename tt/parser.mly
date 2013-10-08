%{

open Std_internal

(* type t =
 *   | Lam of (t s, t) Bind.t
 *   | App of t * t list
 *   | Fun of (t s, t) Bind.t
*)

let cons x a g =
  Term.Cons (Rebind.create (x, Embed.create a) g)

%}

%token Arrow Comma Period Backslash Colon Lparen Rparen Kw_type
%token <Term.Name.t> Var
%token <Unbound.Std.Constant.t> Con

%start term, tele
%type <Term.t> term
%type <Term.t Term.s> tele

%right Arrow

%%

tele
  : Var Colon term             { cons $1 $3 Term.Nil }
  | Var Colon term Comma tele  { cons $1 $3 $5 }
  ;

term
  : Backslash tele Period term
      { Term.Lam (Bind.create $2 $4) }
  | Lparen tele Rparen term
      { Term.Fun (Bind.create $2 $4) }
  | application
      { let (hd, args) = $1 in Term.App (hd, List.rev args)
}
  ;

application
  : atom
      { ($1, []) }
  | application atom
      { let (hd, args) = $1 in (hd, $2 :: args) }
  ;

atom
  : Var     { Term.Var $1 }
  | Con     { Term.Con $1 }
  | Kw_type { Term.Typ (Level.create ()) }
  | Lparen term Rparen { $2 }
  ;

%%

