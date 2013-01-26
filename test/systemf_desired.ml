open Core.Std
open Unbound.Run_time

module Label = String

module rec Self : sig

  module Kind : sig
    type t =
      | Arr of t * t
      | Star
  end

  module Ty_bnd : sig
    type t =
      (Self.Type.t New_name.t) * Kind.t
  end

  module Type : sig
    type t =
      | App of t * t
      | Arr of t * t
      | Exists of (Ty_bnd.t, t) Bind.t
      | Forall of (Ty_bnd.t, t) Bind.t
      | Fun of (Ty_bnd.t, t) Bind.t
      | Name of t New_name.t
      | Record of t Label.Map.t
    module Name : New_name.S with type a := t
  end

  module Term : sig
    type t =
      | App of t * t
      | Dot of t * Label.t
      | Fun of ((t New_name.t) * Type.t, t) Bind.t
      | Let of ((t New_name.t) * (t Embed.t), t) Bind.t
      | Name of t New_name.t
      | Pack of ((Type.t New_name.t) * (Type.t Embed.t), t * Type.t) Bind.t
      | Record of t Label.Map.t
      | Ty_app of t * Type.t
      | Ty_fun of (Ty_bnd.t, t) Bind.t
      | Unpack of ((Type.t New_name.t) * (t New_name.t) * (t Embed.t), t) Bind.t
    module Name : New_name.S with type a := t
  end

end = struct

  module Kind = struct
    type t =
      | Arr of t * t
      | Star
  end

  module Ty_bnd = struct
    type t =
      (Self.Type.t New_name.t) * Kind.t
  end

  module Type = struct
    type t =
      | App of t * t
      | Arr of t * t
      | Exists of (Ty_bnd.t, t) Bind.t
      | Forall of (Ty_bnd.t, t) Bind.t
      | Fun of (Ty_bnd.t, t) Bind.t
      | Name of t New_name.t
      | Record of t Label.Map.t
    module Name = New_name.Make (struct type a = t end)
  end

  module Term = struct
    type t =
      | App of t * t
      | Dot of t * Label.t
      | Fun of ((t New_name.t) * Type.t, t) Bind.t
      | Let of ((t New_name.t) * (t Embed.t), t) Bind.t
      | Name of t New_name.t
      | Pack of (Type.t New_name.t * Type.t Embed.t, t * Type.t) Bind.t
      | Record of t Label.Map.t
      | Ty_app of t * Type.t
      | Ty_fun of (Ty_bnd.t, t) Bind.t
      | Unpack of ((Type.t New_name.t) * (t New_name.t) * (t Embed.t), t) Bind.t
    module Name = New_name.Make (struct type a = t end)
  end

end

