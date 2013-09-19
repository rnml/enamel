open Std_internal

module Term : sig

  type t =
    | Typ of int
    | Var of t Name.t
    | Con of Constant.t
    | Lam of (s, t) Bind.t
    | App of t * t list
    | Fun of (s, t) Bind.t

  and s = (* telescope *)
    | Nil
    | Cons of (t Name.t * t Embed.t, s) Rebind.t

end
