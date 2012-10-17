open Std_internal

module Source (Base : sig
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
    type 'a t
    type 'a check =
      Target.Context.t -> 'a -> Systemf.Term.t * Systemf.Type.t
    val ok : 'a check -> 'a t check
  end
end) : sig

  module Kind : sig
    type t = Base.Kind.t
  end

  module rec Path : sig
    type t = Mod.t
    val ok : Target.Context.t -> t -> Systemf.Term.t * Target.Csig.t
  end

  and Type : sig
    type t =
      | Wrap of t Base.Type.t
      | Path of Path.t
      | Let of Bnd.t * t
    val ok : t Base.Type.check
  end

  and Expr : sig
    type t =
      | Wrap of t Base.Expr.t
      | Path of Path.t
      | Let of Bnd.t * t
    val ok : t Base.Expr.check
  end

  and Sig : sig
    type t =
      | Path of Path.t
      | Val of Type.t
      | Type of Type.t
      | Abstype of Kind.t
      | Sig of Sig.t
      | Struct of Decl.t
      | Fun of Systemf.Term.Name.t * t * t
      | Where of t * Systemf.Label.t list * Type.t
      | Let of Bnd.t * t
    val ok : Target.Context.t -> t -> Target.Asig.t
  end

  and Decl : sig
    type t =
      | Decl of Systemf.Term.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    val ok :
      Target.Context.t
      -> t
      -> (Systemf.Type.Name.t * Systemf.Kind.t) list
       * Target.Csig.t Systemf.Label.Map.t
  end

  and Mod : sig
    type t =
      | Name of Systemf.Term.Name.t
      | Val of Expr.t
      | Type of Type.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * Systemf.Label.t
      | Fun of Systemf.Term.Name.t * Sig.t * t
      | App of t * t
      | Seal of t * Sig.t
      | Let of Bnd.t * t
    val ok : Target.Context.t -> t -> Target.Asig.t * Systemf.Term.t
  end

  and Bnd : sig
    type t =
      | Let of Systemf.Term.Name.t * Mod.t
      | Nil
      | Cat of t * t
      | Include of Mod.t
      | Local of t * t
    val ok :
      Target.Context.t
      -> t
      -> (Systemf.Type.Name.t * Systemf.Kind.t) list
       * Target.Csig.t Systemf.Label.Map.t
       * (Systemf.Term.t -> Systemf.Term.t)
  end

end

