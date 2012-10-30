open Std_internal

module Source (Base : sig
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
    type 'b check =
      Target.Context.t -> 'b -> Systemf.Expr.t * Systemf.Type.t
    val ok : 'a Type.check -> 'b check -> ('a, 'b) t check
  end
end) : sig

  module Kind : sig
    type t = Base.Kind.t with sexp
  end

  module rec Path : sig
    type t = Mod.t with sexp
    val ok : Target.Context.t -> t -> Systemf.Expr.t * Target.Csig.t
  end

  and Type : sig
    type t =
      | Wrap of t Base.Type.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp
    val ok :
      Target.Context.t -> t -> Systemf.Type.t * Systemf.Kind.t
  end

  and Expr : sig
    type t =
      | Wrap of (Type.t, t) Base.Expr.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp
    val ok :
      Target.Context.t -> t -> Systemf.Expr.t * Systemf.Type.t
  end

  and Sig : sig
    type t =
      | Path of Path.t
      | Val of Type.t
      | Type of Type.t
      | Abstype of Kind.t
      | Sig of Sig.t
      | Struct of Decl.t
      | Fun of Systemf.Expr.Name.t * t * t
      | Where of t * Systemf.Label.t list * Type.t
      | Let of Bnd.t * t
    with sexp
    val ok : Target.Context.t -> t -> Target.Asig.t
  end

  and Decl : sig
    type t =
      | Decl of Systemf.Expr.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    with sexp
    val ok :
      Target.Context.t
      -> t
      -> (Systemf.Type.Name.t * Systemf.Kind.t) list
       * Target.Csig.t Systemf.Label.Map.t
  end

  and Mod : sig
    type t =
      | Name of Systemf.Expr.Name.t
      | Val of Expr.t
      | Type of Type.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * Systemf.Label.t
      | Fun of Systemf.Expr.Name.t * Sig.t * t
      | App of t * t
      | Seal of t * Sig.t
      | Let of Bnd.t * t
    with sexp
    val ok :
      (* CR: flip order of returned pair *)
      Target.Context.t -> t -> Target.Asig.t * Systemf.Expr.t
  end

  and Bnd : sig
    type t =
      | Let of Systemf.Expr.Name.t * Mod.t
      | Nil
      | Cat of t * t
      | Include of Mod.t
      | Local of t * t
    with sexp
    val ok :
      Target.Context.t
      -> t
      -> (Systemf.Type.Name.t * Systemf.Kind.t) list
       * Target.Csig.t Systemf.Label.Map.t
       * (Systemf.Expr.t -> Systemf.Expr.t)
  end

end

