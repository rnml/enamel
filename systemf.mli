open Std_internal

module Kind : sig
  type t =
    | Star
    | Arr of t * t
  with sexp
  val equal : t -> t -> bool
end

module Label : Identifiable

module Type : sig

  type t =
    | Name of t New_name.t
    | Arr of t * t
    | Record of t Label.Map.t
    | Forall of t New_name.t * Kind.t * t
    | Exists of t New_name.t * Kind.t * t
    | Fun of t New_name.t * Kind.t * t
    | App of t * t
  with sexp

  module Name : New_name.S with type a := t

  module Context : sig
    type t with sexp
    val empty : t
    val add   : t -> Name.t -> Kind.t -> t
    val find  : t -> Name.t -> Kind.t option
  end

  val fvs : t -> Name.Set.t
  val swap : Name.t * Name.t -> t -> t
  val subst : t -> Name.t * t -> t
  val equal : t -> t -> bool
end

module Expr : sig

  type t =
    | Name of t New_name.t
    | Fun of t New_name.t * Type.t * t
    | App of t * t
    | Record of t Label.Map.t
    | Dot of t * Label.t
    | Ty_fun of Type.Name.t * Kind.t * t
    | Ty_app of t * Type.t
    | Pack of Type.t * t * Type.Name.t * Type.t
    | Unpack of Type.Name.t * t New_name.t * t * t
    | Let of t New_name.t * t * t
  with sexp

  module Name : sig
    include New_name.S with type a := t
    val to_label : t -> Label.t
    val of_label : Label.t -> t
  end

  val type_mod : Type.t -> Kind.t -> t
  val sig_mod : Type.t -> t

  val pack :
    (Type.t * Type.Name.t * Kind.t) list -> t -> Type.t -> t

  val unpack : Type.Name.t list -> Name.t -> t -> t -> t

end

val subtype :
  Type.Context.t
  -> src:Type.t
  -> dst:Type.t
  -> [`Coerce of Expr.t -> Expr.t]

