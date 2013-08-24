open Std_internal

module Ctx = Target.Context

module Kind = struct
  type t = Type with sexp
  let ok _ctx Type = F.Kind.Star
end

module Ty = struct
  type 'a t =
  | Mod of 'a
  | App of 'a t * 'a t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Ty.t * F.Kind.t

  let rec ok check_a ctx = function
    | Mod a -> check_a ctx a
    | App (tf, tx) ->
      let (tf, kf) = ok check_a ctx tf in
      let (tx, kx) = ok check_a ctx tx in
      match kf with
      | F.Kind.Arr (k_dom, k_rng) ->
        if F.Kind.equal k_dom kx
        then (F.Ty.App (tf, tx), k_rng)
        else failwith "kind mismatch"
      | _ -> failwith "applied type of non-arrow kind"

end

module Tm = struct

  type ('a, 'b) t =
    | Mod of 'b
    | Lam of F.Tm.Name.t * 'a Ty.t * ('a, 'b) t
    | App of ('a, 'b) t * ('a, 'b) t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Tm.t * F.Ty.t

  let rec ok : 'a. 'a Ty.check -> 'b check -> ('a, 'b) t check = fun check_ty check_a ctx -> function
    | Mod a -> check_a ctx a
    | App (f, x) ->
      let (f, tf) = ok check_ty check_a ctx f in
      let (x, tx) = ok check_ty check_a ctx x in
      begin
        match tf with
        | F.Ty.Arr (ty_dom, ty_rng) ->
          if F.Ty.equal ty_dom tx
          then (F.Tm.App (f, x), ty_rng)
          else failwith "type mismatch"
        | _ -> failwith "applied term of non-arrow type"
      end
    | Lam (x, dom, body) ->
      (* CR: freshen! *)
      begin
        match Ty.ok check_ty ctx dom with
        | (dom, F.Kind.Star) ->
          let (body, rng) =
            ok check_ty check_a
              (Ctx.add_tm ctx x (Target.Csig.Val dom))
              body
          in
          ( F.Tm.mk_fun x dom body
          , F.Ty.Arr (dom, rng)
          )
        | _ ->
          failwith "lambda argument type annotation of non-star \
            kind"
      end

end

module Fix = struct
  module Ty = struct
    type t = Fix of t Ty.t | Name of F.Ty.Name.t with sexp
    let rec ok ctx = function
      | Fix o -> Ty.ok ok ctx o
      | Name x ->
        match Ctx.find_ty ctx x with
        | None -> failwith "free type var"
        | Some k -> (F.Ty.Name x, k)
  end
  module Tm = struct
    type t =
      | Fix of (Ty.t, t) Tm.t
      | Name of F.Tm.Name.t with sexp
    let rec ok ctx = function
      | Fix e -> Tm.ok Ty.ok ok ctx e
      | Name x ->
        match Ctx.find_tm ctx x with
        | None -> failwith "free type var"
        | Some csig -> (F.Tm.Name x, Target.Csig.to_f csig)
  end
end


