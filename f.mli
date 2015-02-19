(* System F-omega *)

open Std_internal

module Kind : sig
  type t =
    | Type
    | Fun of t * t
  with compare

  val equal : t -> t -> bool
end

module Label : Identifiable

module Type : sig

  module Name : Identifiable

  module Shape : sig
    type 'a t =
      | Name   of Name.t
      | Fun    of 'a * 'a
      | Record of 'a Label.Map.t
      | Forall of Name.t * Kind.t * 'a
      | Exists of Name.t * Kind.t * 'a
      | Lam    of Name.t * Kind.t * 'a
      | App    of 'a * 'a
  end

  type t with compare

  val create : t Shape.t -> t
  val match_ : t -> t Shape.t

  val fv : t -> Name.Set.t
  (* val subst : t -> Name.t * t -> t *)
  val equal : t -> t -> bool
end

module Term : sig

  module Name : Identifiable

  module Shape : sig
    type 'a t =
      | Name   of Name.t
      | Fun    of Name.t * Type.t * 'a
      | App    of 'a * 'a
      | Record of 'a Label.Map.t
      | Dot    of 'a * Label.t
      | Tyfun  of Type.Name.t * Kind.t * 'a
      | Tyapp  of 'a * Type.t
      | Pack   of Type.t * 'a * Type.Name.t * Type.t (* pack <ty, tm> : exists a. ty *)
      | Unpack of Type.Name.t * Name.t * 'a * 'a     (* let pack <a, x> = e in e     *)
      | Let    of Name.t * 'a * 'a                   (* let x = e in e               *)
  end

  type t with compare

  val create : t Shape.t -> t
  val match_ : t -> t Shape.t

  val term_fv : t -> Name.Set.t
  val type_fv : t -> Type.Name.Set.t

  val term_subst : t -> Name.t * t -> t
  val type_subst : t -> Type.Name.t * Type.t -> t

  val equal : t -> t -> bool
end
