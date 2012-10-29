open Std_internal

module Kind : sig
  type t with sexp
  val ok : Target.Context.t -> t -> Systemf.Kind.t
end

module Type : sig
  type 'a t with sexp
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Type.t * Systemf.Kind.t
  val ok : 'a check -> 'a t check
end

module Expr : sig
  type ('a, 'b) t with sexp
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Expr.t * Systemf.Type.t
  val ok : 'a Type.check -> 'b check -> ('a, 'b) t check
end

module Fix : sig
  module Type : sig
    type t with sexp
    val ok : t Type.check
  end
  module Expr : sig
    type t with sexp
    val ok : t Expr.check
  end
end

