open Std_internal

module rec Csig : sig
  type t

  module Shape : sig
    type nonrec 'a t =
       | Val    of F.Type.t
       | Type   of F.Type.t * F.Kind.t
       | Sig    of Asig.t
       | Struct of 'a F.Label.Map.t
       | Fun    of (F.Type.Name.t * F.Kind.t) list * 'a * Asig.t
    val map : 'a1 t -> f:('a1 -> 'a2) -> 'a2 t
  end

  val create : t Shape.t -> t
  val match_ : t -> t Shape.t

  val fv : t -> Name.Set.t
  val equal : t -> t -> bool

end

and Asig : sig
  type t

  module Shape : sig
    type nonrec t =
      | Exists of (F.Type.Name.t * F.Kind.t) list * t
  end

  val create : Shape.t -> t
  val match_ : t -> Shape.t
end
