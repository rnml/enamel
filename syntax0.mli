(* System F-omega *)

open Std_internal

module Label : Identifiable

module F : sig

  module Kind : sig
    type t =
      | Type
      | Fun of t * t
    with compare, sexp

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
        | Lambda of Name.t * Kind.t * 'a
        | App    of 'a * 'a
    end

    type t with compare, sexp

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
        | Lambda of Name.t * Type.t * 'a
        | App    of 'a * 'a
        | Record of 'a Label.Map.t
        | Dot    of 'a * Label.t
        | Tyfun  of Type.Name.t * Kind.t * 'a
        | Tyapp  of 'a * Type.t
        | Pack   of Type.t * 'a * Type.Name.t * Type.t (* pack <ty, tm> : exists a. ty *)
        | Unpack of Type.Name.t * Name.t * 'a * 'a     (* let pack <a, x> = e in e     *)
        | Let    of Name.t * 'a * 'a                   (* let x = e in e               *)
    end

    type t with compare, sexp

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
        | Forall of (F.Type.Name.t * F.Kind.t) list * 'a * Asig.t
      val map : 'a1 t -> f:('a1 -> 'a2) -> 'a2 t
    end

    val create : t Shape.t -> t
    val match_ : t -> t Shape.t

    val equal : t -> t -> bool

  end

  and Asig : sig
    type t

    module Shape : sig
      type t =
        | Exists of (F.Type.Name.t * F.Kind.t) list * Csig.t
    end

    val create : Shape.t -> t
    val match_ : t -> Shape.t
  end

end

module type Uterm = sig
  type t
  val tc : t Unbound_lib.Unbound.Term_tc.t
end

module Fing_modules
         (Base : functor (Path : Term) -> sig
            module Kind : sig type t end
            module Type : sig type t end
            module Term : sig type t end
          end) : sig

  module Label : Identifiable
  module Name : Identifiable

  module rec Type : sig
    type t =
      | Wrap of t Base.Type.t
      | Path of Module.t
      (* | Let  of Struct.t * t *)
  end

  and Term : sig
    type t =
      | Wrap of (Type.t, t) Base.Term.t
      | Path of Module.t
      (* | Let  of Struct.t * t *)
  end

  and Signature : sig
    type t =
      | Path      of Module.t
      | Val       of Type.t
      | Type      of Type.t
      | Abstype   of Base.Kind.t
      | Signature of t
      | Struct    of Decl.t
      | Fun       of Name.t * t * t
      | Where     of t * Label.t list * Type.t
      (* | Let     of Struct.t * t *)
  end

  and Decl : sig
    type t =
      | Decl    of Name.t * Signature.t
      | Nil
      | Cat     of t * t
      | Include of Signature.t
      (* | Local   of Struct.t * t *)
  end

  and Module : sig
    type t =
      | Name      of Name.t
      | Type      of Type.t
      | Value     of Term.t
      | Struct    of Struct.t
      | Signature of Signature.t
      | Dot       of t * Label.t
      | Fun       of Name.t * Signature.t * t
      | AppV      of Name.t * Name.t
      | SealV     of Name.t * Signature.t
      (* | App    of t * t *)
      (* | Seal   of t * Signature.t *)
      (* | Let    of Struct.t * t *)
  end

  and Struct : sig
    type t =
      | Let     of Name.t * Module.t
      | Nil
      | Cat     of t * t
      | Include of Module.t
      (* | Local   of t * t *)
  end

end
