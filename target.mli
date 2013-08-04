open Std_internal

open Unbound
open Systemf

module Args : sig
  type t = (Ty.Name.t * Kind.t Embed.t) list
end

module rec Csig : sig
  type t =
  | Val of Ty.t
  | Type of Ty.t * Kind.t
  | Sig of Asig.t
  | Struct of t Label.Map.t
  | Fun of ((Ty.Name.t * Kind.t Embed.t) list, t * Asig.t) Bind.t

  val mk_fun : (Ty.Name.t * Kind.t) list -> t -> Asig.t -> t

  val un_fun
    :  (Args.t, t * Asig.t) Bind.t
    -> (Ty.Name.t * Kind.t) list * t * Asig.t

  val to_f : t -> Ty.t

  val subst : t -> (Ty.Name.t * Ty.t) -> t

  val matches
    :  Ty.Context.t
    -> t
    -> Asig.t
    -> (Ty.t * Kind.t) list * [`Coerce of Tm.t -> Tm.t]

end

and Asig : sig
  type t =
  | Exists of ((Ty.Name.t * Kind.t Embed.t) list, Csig.t) Bind.t

  val mk_exists : (Ty.Name.t * Kind.t) list -> Csig.t -> t

  val un_exists
    :  (Args.t, Csig.t) Bind.t
    -> (Ty.Name.t * Kind.t) list * Csig.t

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
  val add_tm  : t -> Tm.Name.t -> Csig.t -> t
  val find_tm : t -> Tm.Name.t -> Csig.t option
  val ty_ctx  : t -> Ty.Context.t
end


