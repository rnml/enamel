open Std_internal

module type Base = sig
  module Kind : sig type t end
  module Type : sig type 'ty t end
  module Term : sig type ('tm, 'ty) t end
end

module Source (Base : Base) : sig

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
