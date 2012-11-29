open Core.Std

module Regular = struct
  type 'a t =
  | Option of 'a
  | List   of 'a
  | Sum    of 'a list Constant.Map.t
  | Prod   of 'a list Constant.Map.t
  | Pair   of 'a * 'a
  | Ref    of Name.t
  with sexp
end

module Term = struct
  type ('t, 'p) t =
  | Var (* use site *)
  | Bind of 'p * 't
  with sexp
end

module Pattern = struct
  type ('p, 't) t =
  | Var (* binding site *)
  | Embed  of 't
  | Rebind of 'p * 'p
  | Rec    of 'p
  with sexp
end

type tm =
| Regular of tm Regular.t
| Term of (tm, pt) Term.t

and pt =
| Pregular of pt Regular.t
| Pattern of (pt, tm) Pattern.t

let rec sexp_of_tm = function
  | Regular s -> Regular.sexp_of_t sexp_of_tm s
  | Term t -> Term.sexp_of_t sexp_of_tm sexp_of_pt t

and sexp_of_pt = function
  | Pregular s -> Regular.sexp_of_t sexp_of_pt s
  | Pattern p -> Pattern.sexp_of_t sexp_of_pt sexp_of_tm p

let rec tm_of_sexp sexp =
  try Regular (Regular.t_of_sexp tm_of_sexp sexp)
  with _  -> Term (Term.t_of_sexp tm_of_sexp pt_of_sexp sexp)

and pt_of_sexp sexp =
  try Pregular (Regular.t_of_sexp pt_of_sexp sexp)
  with _  -> Pattern (Pattern.t_of_sexp pt_of_sexp tm_of_sexp sexp)

module Env = struct
  type t = {
    tms : tm Name.Map.t;
    pts : pt Name.Map.t;
  } with sexp
end

