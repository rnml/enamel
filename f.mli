open Std_internal
open Unbound

module Kind : sig
  type t =
    | Star
    | Arr of t * t
  with sexp
  val equal : t -> t -> bool
  val type_rep : t Type.Rep.t
end

module Label : sig
  include Identifiable
  val type_rep : t Type.Rep.t
  val map_type_rep : 'a Type.Rep.t -> 'a Map.t Type.Rep.t
end

module Ty : sig

  type t =
  | Name   of t Name.t
  | Arr    of t * t
  | Record of t Label.Map.t
  | Forall of (t Name.t * Kind.t Embed.t, t) Bind.t
  | Exists of (t Name.t * Kind.t Embed.t, t) Bind.t
  | Fun    of (t Name.t * Kind.t Embed.t, t) Bind.t
  | App    of t * t
  with sexp

  val type_rep : t Type.Rep.t

  module Name : Name.S with type a := t

  val unbind :
    (Name.t * Kind.t Embed.t, t) Bind.t -> Name.t * Kind.t * t

  val forall : Name.t * Kind.t * t -> t
  val exists : Name.t * Kind.t * t -> t
  val fun_   : Name.t * Kind.t * t -> t

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

module Tm : sig

  type t =
  | Name   of t Name.t
  | Fun    of (t Name.t * Ty.t Embed.t, t) Bind.t
  | App    of t * t
  | Record of t Label.Map.t
  | Dot    of t * Label.t
  | Tyfun  of (Ty.Name.t * Kind.t Embed.t, t) Bind.t
  | Tyapp  of t * Ty.t
  | Pack   of (* pack <ty, tm> : exists a. ty *)
      Ty.t * t * (Ty.Name.t, Ty.t) Bind.t
  | Unpack of (Ty.Name.t * t Name.t * t Embed.t, t) Bind.t
  | Let of (t Name.t * t Embed.t, t) Bind.t
  with sexp

  val type_rep : t Type.Rep.t

  val mk_fun : t Name.t -> Ty.t -> t -> t
  val un_fun : (t Name.t * Ty.t Embed.t, t) Bind.t -> t Name.t * Ty.t * t
  val mk_tyfun : Ty.Name.t -> Kind.t -> t -> t
  val un_tyfun : (Ty.Name.t * Kind.t Embed.t, t) Bind.t -> Ty.Name.t * Kind.t * t
  val mk_pack : Ty.t -> t -> Ty.Name.t * Ty.t -> t
  val un_pack : (Ty.Name.t, Ty.t) Bind.t -> Ty.Name.t * Ty.t
  val mk_unpack : Ty.Name.t -> t Name.t -> t -> t -> t
  val un_unpack : (Ty.Name.t * t Name.t * t Embed.t, t) Bind.t -> Ty.Name.t * t Name.t * t * t
  val mk_let : t Name.t -> t -> t -> t
  val un_let : (t Name.t * t Embed.t, t) Bind.t -> t Name.t * t * t

  module Name : sig
    include Name.S with type a := t
    val to_label : t -> Label.t
    val of_label : Label.t -> t
  end

  val type_mod : Ty.t -> Kind.t -> t
  val sig_mod : Ty.t -> t

  val pack : (Ty.t * Ty.Name.t * Kind.t) list -> t -> Ty.t -> t

  val unpack : Ty.Name.t list -> Name.t -> t -> t -> t

end

val subtype : Ty.Context.t -> src:Ty.t -> dst:Ty.t -> [`Coerce of Tm.t -> Tm.t]

