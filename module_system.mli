open Std_internal

module Source (Base : sig
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
  module Tm : sig
    type ('a, 'b) t with sexp
    type 'b check =
      Target.Context.t -> 'b -> Systemf.Tm.t * Systemf.Ty.t
    val ok : 'a Ty.check -> 'b check -> ('a, 'b) t check
  end
end) : sig

  module Kind : sig
    type t = Base.Kind.t with sexp
  end

  module rec Path : sig
    type t = Mod.t with sexp
    val ok : Target.Context.t -> t -> Systemf.Tm.t * Target.Csig.t
  end

  and Ty : sig
    type t =
      | Wrap of t Base.Ty.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp
    val ok :
      Target.Context.t -> t -> Systemf.Ty.t * Systemf.Kind.t
  end

  and Tm : sig
    type t =
      | Wrap of (Ty.t, t) Base.Tm.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp
    val ok :
      Target.Context.t -> t -> Systemf.Tm.t * Systemf.Ty.t
  end

  and Sig : sig
    type t =
      | Path of Path.t
      | Val of Ty.t
      | Type of Ty.t
      | Abstype of Kind.t
      | Sig of Sig.t
      | Struct of Decl.t
      | Fun of Systemf.Tm.Name.t * t * t
      | Where of t * Systemf.Label.t list * Ty.t
      | Let of Bnd.t * t
    with sexp
    val ok : Target.Context.t -> t -> Target.Asig.t
  end

  and Decl : sig
    type t =
      | Decl of Systemf.Tm.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    with sexp
    val ok :
      Target.Context.t
      -> t
      -> (Systemf.Ty.Name.t * Systemf.Kind.t) list
       * Target.Csig.t Systemf.Label.Map.t
  end

  and Mod : sig
    type t =
      | Name of Systemf.Tm.Name.t
      | Val of Tm.t
      | Type of Ty.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * Systemf.Label.t
      | Fun of Systemf.Tm.Name.t * Sig.t * t
      | App of t * t
      | Seal of t * Sig.t
      | Let of Bnd.t * t
    with sexp
    val ok :
      (* CR: flip order of returned pair *)
      Target.Context.t -> t -> Target.Asig.t * Systemf.Tm.t
  end

  and Bnd : sig
    type t =
      | Let of Systemf.Tm.Name.t * Mod.t
      | Nil
      | Cat of t * t
      | Include of Mod.t
      | Local of t * t
    with sexp
    val ok :
      Target.Context.t
      -> t
      -> (Systemf.Ty.Name.t * Systemf.Kind.t) list
       * Target.Csig.t Systemf.Label.Map.t
       * (Systemf.Tm.t -> Systemf.Tm.t)
  end

end

