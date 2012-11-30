open Core.Std

module Def = struct
  type 'a t = [
  | `Synonym of 'a
  | `Variant of 'a list Constant.Map.t
  ] with sexp
end

module Regular = struct
  type 'a t = [
  | `Option of 'a
  | `List   of 'a
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

type term = [ term Regular.t | (term, pattern) Term.t ]
 and pattern = [ pattern Regular.t | (pattern, term) Pattern.t ]
with sexp

module Env = struct
  type t = {
    terms : term Def.t String.Map.t;
    patterns : pattern Def.t String.Map.t;
  } with sexp
end
