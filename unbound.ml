open Core.Std

module Regular = struct
  type 'a t = [
  | `Option of 'a
  | `List   of 'a
  | `Sum    of 'a list Constant.Map.t
  | `Prod   of 'a list Constant.Map.t
  | `Pair   of 'a * 'a
  | `Ref    of String.t
  ] with sexp
end

module Term = struct
  type ('t, 'p) t = [
  | `Var (* use site *)
  | `Bind of 'p * 't
  ] with sexp
end

module Pattern = struct
  type ('p, 't) t = [
  | `Var (* binding site *)
  | `Embed  of 't
  | `Rebind of 'p * 'p
  | `Rec    of 'p
  ] with sexp
end

type tm = [ tm Regular.t | (tm, pt) Term.t ]
 and pt = [ pt Regular.t | (pt, tm) Pattern.t ]
with sexp

module Env = struct
  type t = {
    tms : tm String.Map.t;
    pts : pt String.Map.t;
  } with sexp
end

