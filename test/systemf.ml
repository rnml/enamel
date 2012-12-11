module rec Self : sig

  module Kind : sig
    type t =
      | Arr of Self.Kind.t * Self.Kind.t
      | Star
  end

  module Term : sig
    type t =
      | App of Self.Term.t * Self.Term.t
      | Dot of Self.Term.t * Label.t
      | Fun of ((Term.Name.t)*(Self.Type.t), Self.Term.t) Bind.t
      | Let of ((Term.Name.t)*((Self.Term.t) Embed.t), Self.Term.t) Bind.t
      | Name of Self.Term.Name.t
      | Pack of ((Type.Name.t)*((Self.Type.t) Embed.t), (Self.Term.t)*(Self.Type.t)) Bind.t
      | Record of (Self.Term.t) Label.Map.t
      | Ty_app of Self.Term.t * Self.Type.t
      | Ty_fun of (Self.Ty_bnd.t, Self.Term.t) Bind.t
      | Unpack of ((Type.Name.t)*(Term.Name.t)*((Self.Term.t) Embed.t), Self.Term.t) Bind.t
  end

  module Type : sig
    type t =
      | App of Self.Type.t * Self.Type.t
      | Arr of Self.Type.t * Self.Type.t
      | Exists of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Forall of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Fun of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Name of Self.Type.Name.t
      | Record of (Self.Type.t) Label.Map.t
  end

  module Ty_bnd : sig
    type t =
      (Type.Name.t)*(Self.Kind.t)
  end

end = struct

  module Kind = struct
    type t =
      | Arr of Self.Kind.t * Self.Kind.t
      | Star
  end

  module Term = struct
    type t =
      | App of Self.Term.t * Self.Term.t
      | Dot of Self.Term.t * Label.t
      | Fun of ((Term.Name.t)*(Self.Type.t), Self.Term.t) Bind.t
      | Let of ((Term.Name.t)*((Self.Term.t) Embed.t), Self.Term.t) Bind.t
      | Name of Self.Term.Name.t
      | Pack of ((Type.Name.t)*((Self.Type.t) Embed.t), (Self.Term.t)*(Self.Type.t)) Bind.t
      | Record of (Self.Term.t) Label.Map.t
      | Ty_app of Self.Term.t * Self.Type.t
      | Ty_fun of (Self.Ty_bnd.t, Self.Term.t) Bind.t
      | Unpack of ((Type.Name.t)*(Term.Name.t)*((Self.Term.t) Embed.t), Self.Term.t) Bind.t
  end

  module Type = struct
    type t =
      | App of Self.Type.t * Self.Type.t
      | Arr of Self.Type.t * Self.Type.t
      | Exists of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Forall of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Fun of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Name of Self.Type.Name.t
      | Record of (Self.Type.t) Label.Map.t
  end

  module Ty_bnd = struct
    type t =
      (Type.Name.t)*(Self.Kind.t)
  end

end

