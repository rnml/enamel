(* System F-omega *)

open Std_internal

module Label : Identifiable

module F : sig

  module Kind : sig
    type t =
      | Type
      | Fun of t * t
    with compare

    val equal : t -> t -> bool
  end

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
    val subst : t -> Name.t * t -> t
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

    val subst : t -> Name.t * t -> t
    val type_subst : t -> Type.Name.t * Type.t -> t

    val equal : t -> t -> bool
  end

end

module Target : sig

  module rec Csig : sig
    type t

    module Shape : sig
      type nonrec 'a t =
        | Val    of F.Type.t
        | Type   of F.Type.t * F.Kind.t
        | Sig    of Asig.t
        | Struct of 'a Label.Map.t
        | Fun    of (F.Type.Name.t * F.Kind.t) list * 'a * Asig.t
      val map : 'a1 t -> f:('a1 -> 'a2) -> 'a2 t
    end

    (* val create : t Shape.t -> t *)
    val match_ : t -> t Shape.t

    val equal : t -> t -> bool

  end

  and Asig : sig
    type t

    module Shape : sig
      type nonrec t =
        | Exists of (F.Type.Name.t * F.Kind.t Embed.t) list * Csig.t
    end

    (* val create : Shape.t -> t *)
    (* val match_ : t -> Shape.t *)
  end

end
