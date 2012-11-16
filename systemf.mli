
module Kind : sig
  type t =
    | Star
    | Arr of t * t
  with sexp
  val equal : t -> t -> bool
end

module Label : Name.S

module Type : sig

  module Name : Name.S

  module Context : sig
    type t with sexp
    val empty : t
    val add   : t -> Name.t -> Kind.t -> t
    val find  : t -> Name.t -> Kind.t option
  end

  type t =
    | Name of Name.t
    | Arr of t * t
    | Record of t Label.Map.t
    | Forall of Name.t * Kind.t * t
    | Exists of Name.t * Kind.t * t
    | Fun of Name.t * Kind.t * t
    | App of t * t
  with sexp

  val fvs : t -> Name.Set.t
  val swap : Name.t * Name.t -> t -> t
  val subst : t -> Name.t * t -> t
  val equal : t -> t -> bool
end

module Expr : sig

  module Name : sig
    include Name.S
    val to_label : t -> Label.t
    val of_label : Label.t -> t
  end

  type t =
    | Name of Name.t
    | Fun of Name.t * Type.t * t
    | App of t * t
    | Record of t Label.Map.t
    | Dot of t * Label.t
    | Ty_fun of Type.Name.t * Kind.t * t
    | Ty_app of t * Type.t
    | Pack of Type.t * t * Type.Name.t * Type.t
    | Unpack of Type.Name.t * Name.t * t * t
    | Let of Name.t * t * t
  with sexp

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

