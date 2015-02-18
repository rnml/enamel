open Std_internal

open F

module Args : sig
  type t = (Type.Name.t * Kind.t Embed.t) list
end

module rec Csig : sig

  type t =
    | Val of Type.t
    | Type of Type.t * Kind.t
    | Sig of Asig.t
    | Struct of t Label.Map.t
    | Fun of (Args.t, t * Asig.t) Bind.t

  val mk_fun : (Type.Name.t * Kind.t) list -> t -> Asig.t -> t

  val un_fun : (Args.t, t * Asig.t) Bind.t -> (Type.Name.t * Kind.t) list * t * Asig.t

  val to_f : t -> Type.t

  val subst : t -> (Type.Name.t * Type.t) -> t

  val matches
    :  Type.Context.t
    -> t
    -> Asig.t
    -> (Type.t * Kind.t) list * [`Coerce of Term.t -> Term.t]

end

and Asig : sig

  type t = Exists of (Args.t, Csig.t) Bind.t

  val mk_exists : (Type.Name.t * Kind.t) list -> Csig.t -> t

  val un_exists : (Args.t, Csig.t) Bind.t -> (Type.Name.t * Kind.t) list * Csig.t

  val to_f : t -> Type.t

  val subst : t -> Type.Name.t * Type.t -> t
end

module Context : sig
  type t with sexp
  val empty : t
  (* CR: change the type of add_ty to return the Type.Name rather than
     take it in *)
  val add_ty  : t -> Type.Name.t -> Kind.t -> t
  val find_ty : t -> Type.Name.t -> Kind.t option
  val add_tm  : t -> Term.Name.t -> Csig.t -> t
  val find_tm : t -> Term.Name.t -> Csig.t option
  val ty_ctx  : t -> Type.Context.t
end
