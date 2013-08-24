open Std_internal

module Kind : sig
  type t with sexp
  val ok : Target.Context.t -> t -> F.Kind.t
end

module Ty : sig
  type 'a t with sexp
  type 'a check =
    Target.Context.t -> 'a -> F.Ty.t * F.Kind.t
  val ok : 'a check -> 'a t check
end

module Tm : sig
  type ('a, 'b) t with sexp
  type 'a check =
    Target.Context.t -> 'a -> F.Tm.t * F.Ty.t
  val ok : 'a Ty.check -> 'b check -> ('a, 'b) t check
end

module Fix : sig
  module Ty : sig
    type t with sexp
    val ok : t Ty.check
  end
  module Tm : sig
    type t with sexp
    val ok : t Tm.check
  end
end

