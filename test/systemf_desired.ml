open Core.Std
open Unbound.Run_time

module Label = String

module rec Self : sig

  module Kind : sig
    type t =
      | Arr of Self.Kind.t * Self.Kind.t
      | Star
  end

  module Ty_bnd : sig
    type t =
      (Self.Type.t New_name.t) * (Self.Kind.t)
  end

  module Type : sig
    module Name : New_name.S with type a = t
    type t =
      | App of Self.Type.t * Self.Type.t
      | Arr of Self.Type.t * Self.Type.t
      | Exists of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Forall of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Fun of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Name of Self.Type.t New_name.t
      | Record of (Self.Type.t) Label.Map.t
  end

  module Term : sig
    module Name : New_name.S with type a = t
    type t =
      | App of Self.Term.t * Self.Term.t
      | Dot of Self.Term.t * Label.t
      | Fun of ((Self.Term.t New_name.t) * (Self.Type.t), Self.Term.t) Bind.t
      | Let of ((Self.Term.t New_name.t) * ((Self.Term.t) Embed.t), Self.Term.t) Bind.t
      | Name of Self.Term.t New_name.t
      | Pack of ((Self.Type.t New_name.t) * ((Self.Type.t) Embed.t), (Self.Term.t) * (Self.Type.t)) Bind.t
      | Record of (Self.Term.t) Label.Map.t
      | Ty_app of Self.Term.t * Self.Type.t
      | Ty_fun of (Self.Ty_bnd.t, Self.Term.t) Bind.t
      | Unpack of ((Self.Type.t New_name.t) * (Self.Term.t New_name.t) * ((Self.Term.t) Embed.t), Self.Term.t) Bind.t
  end

end = struct

  module Kind = struct
    type t =
      | Arr of Self.Kind.t * Self.Kind.t
      | Star
  end

  module Ty_bnd = struct
    type t =
      (Self.Type.t New_name.t) * (Self.Kind.t)
  end

  module Type = struct
    module Name = New_name.Make (struct end)
    type t =
      | App of Self.Type.t * Self.Type.t
      | Arr of Self.Type.t * Self.Type.t
      | Exists of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Forall of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Fun of (Self.Ty_bnd.t, Self.Type.t) Bind.t
      | Name of Self.Type.t New_name.t
      | Record of (Self.Type.t) Label.Map.t
  end

  module Term = struct
    module Name = New_name.Make (struct end)
    type t =
      | App of Self.Term.t * Self.Term.t
      | Dot of Self.Term.t * Label.t
      | Fun of ((Self.Term.t New_name.t) * (Self.Type.t), Self.Term.t) Bind.t
      | Let of ((Self.Term.t New_name.t) * ((Self.Term.t) Embed.t), Self.Term.t) Bind.t
      | Name of Self.Term.t New_name.t
      | Pack of ((Self.Type.t New_name.t) * ((Self.Type.t) Embed.t), (Self.Term.t) * (Self.Type.t)) Bind.t
      | Record of (Self.Term.t) Label.Map.t
      | Ty_app of Self.Term.t * Self.Type.t
      | Ty_fun of (Self.Ty_bnd.t, Self.Term.t) Bind.t
      | Unpack of ((Self.Type.t New_name.t) * (Self.Term.t New_name.t) * ((Self.Term.t) Embed.t), Self.Term.t) Bind.t
  end

end

