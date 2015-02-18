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

  type t

  val create : t Shape.t -> t
  val match_ : t -> t Shape.t

  val fv : t -> Name.Set.t
  (* val subst : t -> Name.t * t -> t *)
  val equal : t -> t -> bool

  module Context : sig
    type t
    val empty : t
    val add   : t -> Name.t -> Kind.t -> t
    val find  : t -> Name.t -> Kind.t option
  end
end

(* module Term : sig
 *
 *   module Name : T
 *
 *   module Shape : sig
 *     type 'a t =
 *       | Name   of Name.t
 *       | Fun    of Name.t * Type.t * 'a
 *       | App    of 'a * 'a
 *       | Record of 'a Label.Map.t
 *       | Dot    of 'a * Label.t
 *       | Tyfun  of Type.Name.t * Kind.t * 'a
 *       | Tyapp  of 'a * Type.t
 *       | Pack   of Type.t * 'a * Type.Name.t * Type.t (\* pack <ty, tm> : exists a. ty *\)
 *       | Unpack of Type.Name.t * Name.t * 'a * 'a
 *       | Let    of Name.t * 'a * 'a
 *   end
 *
 *   type t
 *
 *   val create : t Shape.t -> t
 *   val match_ : t -> t Shape.t
 *
 *   module Name : sig
 *     include Name.S with type a := t
 *     val to_label : t -> Label.t
 *     val of_label : Label.t -> t
 *   end
 *
 *   val type_mod : Type.t -> Kind.t -> t
 *   val sig_mod : Type.t -> t
 *
 *   module Context : sig
 *     type t
 *     val empty   : t
 *     val add_tm  : t -> Name.t -> Type.t -> t
 *     val find_tm : t -> Name.t -> Type.t option
 *     val add_ty  : t -> Type.Name.t -> Kind.t -> t
 *     val ty_ctx  : t -> Type.Context.t
 *   end
 * end
 *)
