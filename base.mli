open Std_internal

module Kind : sig
  type t
  val ok : Target.Context.t -> t -> Systemf.Kind.t
end

module Type : sig
  type 'a t
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Type.t * Systemf.Kind.t
  val ok : 'a check -> 'a t check
end

module Expr : sig
  type ('a, 'b) t
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Term.t * Systemf.Type.t
  val ok : 'a Type.check -> 'b check -> ('a, 'b) t check
end
