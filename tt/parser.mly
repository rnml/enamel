%{

open Std_internal

let cons x a g = Term.Cons (Rebind.create (x, Embed.create a) g)

%}

%token Arrow Comma Period Backslash Colon Kw_type
%token Lparen Rparen
%token Semi EOF
%token <Term.Name.t> Var
%token <Unbound.Std.Constant.t> Con

%start term_top, tele_top
%type <Term.t> term_top
%type <Term.t Term.s> tele_top

%right Arrow

%%

term_top : term EOF { $1 } ;

tele_top
  : EOF                          { Term.Nil }
  | Var Colon term Semi tele_top { cons $1 $3 $5 }
  ;

tele
  :                            { Term.Nil }
  | Var Colon term             { cons $1 $3 Term.Nil }
  | Var Colon term Comma tele  { cons $1 $3 $5 }
  ;

term
  : Backslash tele Period term
      { Term.Lam (Bind.create $2 $4) }
  | Lparen tele Rparen Arrow term
      { Term.Fun (Bind.create $2 $5) }
  | application
      { let (hd, args) = $1 in
        if List.is_empty args
        then hd
        else Term.App (hd, List.rev args)
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

