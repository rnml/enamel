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

module Term : sig
  type ('a, 'b) t
  type 'a check =
    Target.Context.t -> 'a -> Systemf.Term.t * Systemf.Type.t
  val ok : 'a Type.check -> 'b check -> ('a, 'b) t check
end

module Fix : sig
  module Type : sig
    type t with sexp
    val ok : t Type.check
  end
  module Term : sig
    type t with sexp
    val ok : t Term.check
  end
end

