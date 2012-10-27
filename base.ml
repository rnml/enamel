open Std_internal

module F = Systemf

module Ctx = Target.Context

module Kind = struct
  type t = Type with sexp
  let ok _ctx Type = F.Kind.Star
end

module Type = struct
  type 'a t =
  | Mod of 'a
  | App of 'a t * 'a t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Type.t * F.Kind.t

  let ref_name = F.Type.Name.freshen (F.Type.Name.raw "ref")
  let int_name = F.Type.Name.freshen (F.Type.Name.raw "int")

  let rec ok check_a ctx = function
    | Mod a -> check_a ctx a
    | App (tf, tx) ->
      let (tf, kf) = ok check_a ctx tf in
      let (tx, kx) = ok check_a ctx tx in
      match kf with
      | F.Kind.Arr (k_dom, k_rng) ->
        if F.Kind.equal k_dom kx
        then (F.Type.App (tf, tx), k_rng)
        else failwith "kind mismatch"
      | _ -> failwith "applied type of non-arrow kind"

end

module Fix = struct
  module Type = struct
    type t = Fix of t Type.t | Name of F.Type.Name.t with sexp
    let rec ok ctx = function
      | Fix o -> Type.ok ok ctx o
      | Name x ->
        match Ctx.find_ty ctx x with
        | None -> failwith "free type var"
        | Some k -> (F.Type.Name x, k)
  end
end

module Expr = struct

  type ('a, 'b) t =
    | Mod of 'b
    | Lam of Name.t * 'a Type.t * ('a, 'b) t
    | App of ('a, 'b) t * ('a, 'b) t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Term.t * F.Type.t

  let rec ok _ _ _ = assert false
  (* let rec ok check_a check_ty ctx = function
   *   | Mod a -> check_a ctx a
   *   | Lam (x, typ, b) -> *)

end


