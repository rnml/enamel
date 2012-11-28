open Core.Std

module Type = struct
  type t =
  | Var
  | Alt of (Constant.t * t list) list
  | Ref of Name.t
  | Bind of p * t
  and p =
  | Pvar
  | Palt of (Constant.t * t list) list
  | Pref of Name.t
  | Embed of t
  | Rebind of p * p
  | Rec of p
  with sexp
end

module Term = struct
  type 'a t =
  | Hole of 'a
  | Lam of ('a t -> 'a t)
  | App of 'a t * 'a t

  let rec whnf = function
    | App (Lam fn, arg) -> whnf (fn arg)
    | x -> x

end

(* let free_vars *)

