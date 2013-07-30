open Std_internal

module Kind : sig
  type t with sexp
  val ok : Target.Context.t -> t -> Systemf.Kind.t
end

module Ty : sig
  type 'a t with sexp
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Ty.t * Systemf.Kind.t
  val ok : 'a check -> 'a t check
end

module Expr : sig
  type ('a, 'b) t with sexp
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Expr.t * Systemf.Ty.t
  val ok : 'a Ty.check -> 'b check -> ('a, 'b) t check
end

module Fix : sig
  module Ty : sig
    type t with sexp
    val ok : t Ty.check
  end
  module Expr : sig
    type t with sexp
    val ok : t Expr.check
  end
end

