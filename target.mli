open Std_internal

open Systemf

module rec Csig : sig
  type t =
    | Val of Ty.t
    | Type of Ty.t * Kind.t
    | Sig of Asig.t
    | Struct of t Label.Map.t
    | Fun of (Ty.Name.t * Kind.t) list * t * Asig.t
  val to_f : t -> Ty.t
  val subst : t -> (Ty.Name.t * Ty.t) -> t
  val matches :
    Ty.Context.t -> t -> Asig.t ->
      (Ty.t * Kind.t) list * [`Coerce of Expr.t -> Expr.t]
end

and Asig : sig
  type t = Exists of (Ty.Name.t * Kind.t) list * Csig.t
  val to_f : t -> Ty.t
  val subst : t -> Ty.Name.t * Ty.t -> t
end

module Context : sig
  type t with sexp
  val empty : t
  (* CR: change the type of add_ty to return the Ty.Name rather than
     take it in *)
  val add_ty  : t -> Ty.Name.t -> Kind.t -> t
  val find_ty : t -> Ty.Name.t -> Kind.t option
  val add_tm  : t -> Expr.Name.t -> Csig.t -> t
  val find_tm : t -> Expr.Name.t -> Csig.t option
  val ty_ctx  : t -> Ty.Context.t
end


