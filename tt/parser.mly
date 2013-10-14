%{

open Std_internal

let cons x a g = Term.Cons (Rebind.create (x, Embed.create a) g)

let un_fun =
  let rec loop acc = function
    | Term.Fun b ->
      let (xas, t) = Term.unbind b in
      loop (xas :: acc) t
    | other -> ([], other)
  in
  fun t ->
    let (args, body) = loop [] t in
    (List.concat (List.rev args), body)

let con_arg ~tycon ty =
  let (arg_args, arg) = un_fun ty in
  match arg with
  | Term.App (Term.Con c, inds)
    when Constant.equal c tycon -> Ind_type.Rec (Term.Binds.bind (arg_args, inds))
  | Term.Con c
    when Constant.equal c tycon -> Ind_type.Rec (Term.Binds.bind (arg_args, []))
  | _ -> Ind_type.Nonrec ty

let con_ty ~tycon ~tmcon x =
  let (args, result) = un_fun x in
  match result with
  | Term.Con c when Constant.equal c tycon ->
    Term.Binds.bind (args, [])
  | Term.App (Term.Con c, inds) when Constant.equal c tycon ->
    let args = List.map args ~f:(fun (x, arg) -> (x, con_arg ~tycon arg)) in
    Term.Binds.bind (args, inds)
  | _ -> failwiths "invalid return type for constructor" tmcon Constant.sexp_of_t

let mk_kind ty =
  let (indices, result) = un_fun ty in
  match result with
  | Term.Typ level -> Term.Binds.bind (indices, level)
  | _ -> failwithf "kind of inductive type declaration does not end in Type" ()

%}

%token Arrow Comma Equal Period Backslash Colon Pipe
%token Kw_type Kw_data
%token Lparen Rparen
%token Semi EOF
%token <Term.Name.t> Var
%token <Unbound.Std.Constant.t> Con

%start term_top, tele_top ind_type_top
%type <Term.t> term_top
%type <Term.t Term.s> tele_top
%type <Ind_type.t> ind_type_top

%right Arrow

%%

ind_type_top : ind_type EOF { $1 } ;

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

tele_opt
  : /* nothing */      { Term.Nil }
  | Lparen tele Rparen { $2 }
  ;

constructors
  :
      { fun _ -> [] }
  | Pipe Con Colon term constructors
      { fun tycon -> ($2, con_ty ~tycon ~tmcon:$2 $4) :: $5 tycon }
  ;

ind_type
  : Kw_data Con tele_opt Colon term Equal constructors {
        let tycon = $2 in
        Bind.create $3 {
         Ind_type.
         tycon;
         kind = mk_kind $5;
         cons = Constant.Map.of_alist_exn ($7 tycon);
        }
      }
  ;

%%

