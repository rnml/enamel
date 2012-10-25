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
    | Fun of 'a t * 'a t
    | Pair of 'a t * 'a t
    | Int
    | Ref of 'a t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Type.t * F.Kind.t

  let ref_name = F.Type.Name.freshen (F.Type.Name.raw "ref")
  let int_name = F.Type.Name.freshen (F.Type.Name.raw "int")

  let rec ok check_a ctx = function
    | Mod a -> check_a ctx a
    | Fun (a, b) ->
      begin
        match ok check_a ctx a with
        | (a, F.Kind.Star) ->
          begin
            match ok check_a ctx b with
            | (b, F.Kind.Star) -> (F.Type.Arr (a, b), F.Kind.Star)
            | _ -> failwith "range type not of kind 'Type'"
          end
        | _ -> failwith "domain type not of kind 'Type'"
      end
    | Pair (a, b) ->
      begin
        match ok check_a ctx a with
        | (a, F.Kind.Star) ->
          begin
            match ok check_a ctx b with
            | (b, F.Kind.Star) -> (F.Type.Arr (a, b), F.Kind.Star)
            | _ -> failwith "second type in pair not of kind 'Type'"
          end
        | _ -> failwith "first type in pair not of kind 'Type'"
      end
    | Ref a ->
      begin
        match ok check_a ctx a with
        | (a, F.Kind.Star) ->
          (F.Type.App (F.Type.Name ref_name, a), F.Kind.Star)
        | _ -> failwith "argument type of Ref not of kind 'Type'"
      end
    | Int ->
      (F.Type.Name int_name, F.Kind.Star)
end

module Expr = struct

  type ('a, 'b) t =
    | Mod of 'b
    | Lam of Name.t * 'a Type.t * ('a, 'b) t
    | App of ('a, 'b) t * ('a, 'b) t
    | Pair of ('a, 'b) t * ('a, 'b) t
    | Fst of ('a, 'b) t
    | Snd of ('a, 'b) t
    | Zero
    | One
    | Plus of ('a, 'b) t * ('a, 'b) t
    | Minus of ('a, 'b) t * ('a, 'b) t
    | Times of ('a, 'b) t * ('a, 'b) t
    | Ref of ('a, 'b) t
    | Deref of ('a, 'b) t
    | Assign of ('a, 'b) t * ('a, 'b) t
  with sexp

  type 'a check = Ctx.t -> 'a -> F.Term.t * F.Type.t

  let rec ok _ _ _ = assert false
  (* let rec ok check_a check_ty ctx = function
   *   | Mod a -> check_a ctx a
   *   | Lam (x, typ, b) -> *)

end


