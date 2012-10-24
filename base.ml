open Std_internal

module F = Systemf

module Ctx = Target.Context

module Kind = struct
  type t = Type with sexp
  let ok _ctx Type = F.Kind.Star
end

module Type = struct
  type 'a t =
    | Mod of 'a
    | Fun of 'a t * 'a t
    | Pair of 'a t * 'a t
    | Int
    | Ref of 'a t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Type.t * F.Kind.t

  let ok _check_a _ctx _t =
    assert false
end

module Expr = struct
  type 'a t =
    | Mod of 'a
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Term.t * F.Type.t

  let ok _check_a _ctx _t =
    assert false
end

