open Std_internal

(* CR: always normalize signatures by using set-binders *)

module F = Systemf

module Ctx = Target.Context

module Source (Base : sig
  module Kind : sig
    type t with sexp
    val ok : Ctx.t -> t -> F.Kind.t
  end
  module Type : sig
    type 'a t with sexp
    type 'a check = Ctx.t -> 'a -> F.Type.t * F.Kind.t
    val ok : 'a check -> 'a t check
  end
  module Expr : sig
    type ('a, 'b) t with sexp
    type 'b check = Ctx.t -> 'b -> F.Expr.t * F.Type.t
    val ok : 'a Type.check -> 'b check -> ('a, 'b) t check
  end
end) = struct

  module Kind = Base.Kind

  module rec Path : sig
    type t = Mod.t with sexp
    val ok : Ctx.t -> t -> F.Expr.t * Target.Csig.t
  end = struct
    type t = Mod.t with sexp
    let ok ctx m =
      let (Target.Asig.Exists (alphas, csig), e) = Mod.ok ctx m in
      if
        let fvs = F.Type.fvs (Target.Csig.to_f csig) in
        List.exists alphas ~f:(fun (a, _k) -> Set.mem fvs a)
      then
         failwith "path projection lets type variable escape"
      else begin
        let x = F.Expr.Name.raw "path.project" in
        ( List.fold alphas ~init:e ~f:(fun acc (a, _) ->
            F.Expr.Unpack (a, x, acc, F.Expr.Name x))
        , csig )
      end
  end

  and Type : sig
    type t =
      | Wrap of t Base.Type.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp
    val ok : t Base.Type.check
  end = struct

    type t =
      | Wrap of t Base.Type.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp

    let rec ok ctx t =
      match t with
      | Wrap typ -> Base.Type.ok ok ctx typ
      | Path p ->
        let (_, csig) = Path.ok ctx p in
        (match csig with
        | Target.Csig.Type (t, k) -> (t, k)
        | _ -> failwith "non-type module embedded in type expression")
      | Let (b, t) -> (* derived form *)
        let x = F.Expr.Name.raw "local.type" in
        let lx = F.Expr.Name.to_label x in
        ok ctx
          (Path (Mod.Dot (Mod.Struct (Bnd.Cat (b, Bnd.let_typ x t)), lx)))

  end

  and Expr : sig
    type t =
      | Wrap of (Type.t, t) Base.Expr.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp
    val ok : t Base.Expr.check
  end = struct

    type t =
      | Wrap of (Type.t, t) Base.Expr.t
      | Path of Path.t
      | Let of Bnd.t * t
    with sexp

    let rec ok ctx e =
      match e with
      | Wrap exp -> Base.Expr.ok Type.ok ok ctx exp
      | Path p ->
        let (e, csig) = Path.ok ctx p in
        (match csig with
        | Target.Csig.Val t -> (e, t)
        | _ -> failwith "non-term module embedded in term expression")
      | Let (b, e) -> (* derived form *)
        let x = F.Expr.Name.raw "local.expr" in
        let lx = F.Expr.Name.to_label x in
        ok ctx (Path (Mod.Dot (Mod.Struct (Bnd.Cat (b, Bnd.let_val x e)), lx)))

  end

  and Sig : sig
    type t =
      | Path of Path.t
      | Val of Type.t
      | Type of Type.t
      | Abstype of Kind.t
      | Sig of Sig.t
      | Struct of Decl.t
      | Fun of F.Expr.Name.t * t * t
      | Where of t * F.Label.t list * Type.t
      | Let of Bnd.t * t
    with sexp
    val ok : Ctx.t -> t -> Target.Asig.t
  end = struct

    type t =
      | Path of Path.t
      | Val of Type.t
      | Type of Type.t
      | Abstype of Kind.t
      | Sig of Sig.t
      | Struct of Decl.t
      | Fun of F.Expr.Name.t * t * t
      | Where of t * F.Label.t list * Type.t
      | Let of Bnd.t * t
    with sexp

    let rec ok ctx = function
      | Path p ->
        let (_e, csig) = Path.ok ctx p in
        begin
          match csig with
          | Target.Csig.Sig asig -> asig
          | _ -> failwith "non-signature module expression \
                           where a signature was expected"
        end
      | Val t ->
        let (t, k) = Type.ok ctx t in
        begin
          match k with
          | F.Kind.Star -> ()
          | _ -> failwith "improper type"
        end;
        Target.Asig.Exists ([], Target.Csig.Val t)
      | Type t ->
        let (t, k) = Type.ok ctx t in
        Target.Asig.Exists ([], Target.Csig.Type (t, k))
      | Abstype k ->
        let k = Kind.ok ctx k in
        let a = assert false (* F.Type.Name.dummy *) in
        let t = F.Type.Name a in
        Target.Asig.Exists ([(a, k)], Target.Csig.Type (t, k))
      | Sig s ->
        let asig = Sig.ok ctx s in
        Target.Asig.Exists ([], Target.Csig.Sig asig)
      | Struct d ->
        let (alphas, map) = Decl.ok ctx d in
        Target.Asig.Exists
          ( alphas
          , Target.Csig.Struct map )
      | Fun (x, s, s') ->
        let Target.Asig.Exists (aks, csig) = Sig.ok ctx s in
        let ctx =
          List.fold aks ~init:ctx ~f:(fun ctx (a, k) ->
            Target.Context.add_ty ctx a k)
        in
        let ctx = Target.Context.add_tm ctx x csig in
        let asig = Sig.ok ctx s' in
        Target.Asig.Exists ([], Target.Csig.Fun (aks, csig, asig))
      | Where (s, path, t) ->
        let Target.Asig.Exists (aks, csig_orig) = Sig.ok ctx s in
        let (t, k) = Type.ok ctx t in
        let rec loop csig = function
          | l :: ls ->
            (match csig with
            | Target.Csig.Struct map ->
              (match F.Label.Map.find map l with
              | Some csig -> loop csig ls
              | None -> failwith "bad path in where constraint (type 1)")
            | _ -> failwith "bad path in where constraint (type 2)")
          | [] ->
            (match csig with
            | Target.Csig.Type (t', k') ->
              assert (F.Kind.equal k k');
              (match t' with
              | F.Type.Name a ->
                let alphas =
                  List.filter aks ~f:(fun (a', _) ->
                    not (F.Type.Name.equal a a'))
                in
                Target.Asig.Exists
                  ( alphas
                  , Target.Csig.subst csig_orig (a, t) )
              | _ -> failwith "where constraint mentions non-abstract type")
            | _ -> failwith "bad path in where constraint (type 3)")
        in
        loop csig_orig path

      | Let (b, e) -> (* derived form *)
        let x = F.Expr.Name.raw "local.sig" in
        let lx = F.Expr.Name.to_label x in
        ok ctx
          (Path (Mod.Dot (Mod.Struct (Bnd.Cat (b, Bnd.let_sig x e)), lx)))
  end

  and Decl : sig

    type t =
      | Decl of F.Expr.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    with sexp

    val ok :
      Ctx.t
      -> t
      -> (F.Type.Name.t * F.Kind.t) list
       * Target.Csig.t F.Label.Map.t

  end = struct

    type t =
      | Decl of F.Expr.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    with sexp

    let rec ok ctx = function
      | Decl (x, s) ->
        let (Target.Asig.Exists (aks, csig)) = Sig.ok ctx s in
        let lx = F.Expr.Name.to_label x in
        ( aks
        , F.Label.Map.singleton lx csig )
      | Nil ->
        ( []
        , F.Label.Map.empty )
      | Cat (d1, d2) ->
        let (alphas1, map1) = ok ctx d1 in
        let ctx =
          List.fold ~init:ctx alphas1
            ~f:(fun ctx (a, k) -> Ctx.add_ty ctx a k)
        in
        let ctx =
          Map.fold ~init:ctx map1
            ~f:(fun ~key:lx ~data:csig ctx ->
              let x = F.Expr.Name.of_label lx in
              Ctx.add_tm ctx x csig)
        in
        let (alphas2, map2) = ok ctx d2 in
        ( alphas1 @ alphas2
        , Map.merge map1 map2 (fun ~key:_ ys ->
            match ys with
            | `Left x -> Some x
            | `Right x -> Some x
            | `Both (_, x) -> Some x))
      | Include s ->
        let Target.Asig.Exists (aks, csig) = Sig.ok ctx s in
        let map =
          match csig with
          | Target.Csig.Struct map -> map
          | _ -> assert false
        in
        (aks, map)
      | Local (b, d) -> (* derived form *)
        ok ctx (Include (Sig.Let (b, Sig.Struct d)))

  end

  and Mod : sig
    type t =
      | Name of F.Expr.Name.t
      | Val of Expr.t
      | Type of Type.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * F.Label.t
      | Fun of F.Expr.Name.t * Sig.t * t
      | App of t * t
      | Seal of t * Sig.t
      | Let of Bnd.t * t
    with sexp
    val ok : Ctx.t -> t -> Target.Asig.t * F.Expr.t
  end = struct

    type t =
      | Name of F.Expr.Name.t
      | Val of Expr.t
      | Type of Type.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * F.Label.t
      | Fun of F.Expr.Name.t * Sig.t * t
      | App of t * t
      | Seal of t * Sig.t
      | Let of Bnd.t * t
    with sexp

    let lookup ctx x =
      match Ctx.find_tm ctx x with
      | None -> failwith "unknown variable"
      | Some csig -> csig

    let rec ok ctx t =
      match t with
      | Name x ->
        (match Ctx.find_tm ctx x with
        | None ->
          failwithf "unbound var %s" (F.Expr.Name.to_string x) ()
        | Some csig -> (Target.Asig.Exists ([], csig), F.Expr.Name x))
      | Val e ->
        let (e, t) = Expr.ok ctx e in
        ( Target.Asig.Exists ([], Target.Csig.Val t)
        , e )
      | Type t ->
        let (t, k) = Type.ok ctx t in
        ( Target.Asig.Exists ([], Target.Csig.Type (t, k))
        , F.Expr.type_mod t k )
      | Sig s ->
        let asig = Sig.ok ctx s in
        ( Target.Asig.Exists ([], Target.Csig.Sig asig)
        , F.Expr.sig_mod (Target.Asig.to_f asig) )
      | Struct bnd ->
        let (aks, map, bnd) = Bnd.ok ctx bnd in
        let asig =
          Target.Asig.Exists (aks, Target.Csig.Struct map)
        in
        ( asig
        , bnd (F.Expr.Record
            (Map.mapi map ~f:(fun ~key:x ~data:_ ->
              F.Expr.Name (F.Expr.Name.of_label x)))) )
      | Dot (m, x) ->
        let (Target.Asig.Exists (alphas, csig), e) = ok ctx m in
        begin
          match csig with
          | Target.Csig.Struct map ->
            (match F.Label.Map.find map x with
            | None -> failwith "unknown field"
            | Some xsig ->
              (Target.Asig.Exists (alphas, xsig), F.Expr.Dot (e, x)))
          | _ -> failwith "field projection from non-struct"
        end
      | Fun (x, s, m) ->
        let Target.Asig.Exists (aks, csig) = Sig.ok ctx s in
        let ctx =
          List.fold aks ~init:ctx ~f:(fun ctx (a, k) ->
            Target.Context.add_ty ctx a k)
        in
        let ctx = Target.Context.add_tm ctx x csig in
        let (asig, e) = ok ctx m in
        ( Target.Asig.Exists ([], Target.Csig.Fun (aks, csig, asig))
        , List.fold_right aks
            ~f:(fun (a, k) acc -> F.Expr.Ty_fun (a, k, acc))
            ~init:(F.Expr.Fun (x, Target.Csig.to_f csig, e)) )
      | App (Name x1, Name x2) ->
        let (aks, csig', asig) =
          match lookup ctx x1 with
          | Target.Csig.Fun (aks, csig', asig) -> (aks, csig', asig)
          | _ -> failwith "applied non-function"
        in
        let csig = lookup ctx x2 in
        let (tks, `Coerce f) =
          Target.Csig.matches (Ctx.ty_ctx ctx) csig
            (Target.Asig.Exists (aks, csig'))
        in
        let subs =
          List.zip_exn (List.map ~f:fst aks) (List.map ~f:fst tks)
        in
        (* CR: does this require simultaneous substitution? *)
        ( List.fold ~f:Target.Asig.subst ~init:asig subs
        , F.Expr.App
            ( List.fold ~init:(F.Expr.Name x1) tks
                ~f:(fun acc (t, _k) -> F.Expr.Ty_app (acc, t))
            , f (F.Expr.Name x2)) )
      | Seal (Name x, s) ->
        let csig = lookup ctx x in
        let asig = Sig.ok ctx s in
        let (tks, `Coerce f) =
          Target.Csig.matches (Ctx.ty_ctx ctx) csig asig
        in
        (match asig with
        | Target.Asig.Exists (alphas, csig') ->
          let taks =
            List.map (List.zip_exn alphas tks)
              ~f:(fun ((a, k), (t, _k)) -> (t, a, k))
          in
          (* CR: something is wrong, we never unpack! *)
          ( asig
          , F.Expr.pack taks (f (F.Expr.Name x)) (Target.Csig.to_f csig') ))
      | Let (b, m) -> (* derived form *)
        let x = assert false (* F.Expr.Name.dummy *) in
        let lx = F.Expr.Name.to_label x in
        let m =  Dot (Struct (Bnd.Cat (b, Bnd.Let (x, m))), lx) in
        ok ctx m
      | App (m1, m2) -> (* derived form *)
        let x1 = F.Expr.Name.raw "fun" in
        let x2 = F.Expr.Name.raw "arg" in
        let m =
          Let
            ( Bnd.Cat (Bnd.Let (x1, m1), Bnd.Let (x2, m2))
            , App (Name x1, Name x2) )
        in
        ok ctx m
      | Seal (m, s) -> (* derived form *)
        let x = F.Expr.Name.raw "conc" in
        let m = Let (Bnd.Let (x, m), Seal (Name x, s)) in
        ok ctx m
  end

  and Bnd : sig

    type t =
      | Let of F.Expr.Name.t * Mod.t
      | Nil
      | Cat of t * t
      | Include of Mod.t
      | Local of t * t
    with sexp

    val let_val : F.Expr.Name.t -> Expr.t -> t
    val let_typ : F.Expr.Name.t -> Type.t -> t
    val let_sig : F.Expr.Name.t -> Sig.t -> t

    val ok :
      Ctx.t
      -> t
      -> (F.Type.Name.t * F.Kind.t) list
       * Target.Csig.t F.Label.Map.t
       * (F.Expr.t -> F.Expr.t)

  end = struct

    type t =
      | Let of F.Expr.Name.t * Mod.t
      | Nil
      | Cat of t * t
      | Include of Mod.t
      | Local of t * t
    with sexp

    let let_val x e = Let (x, Mod.Val e)
    let let_typ x t = Let (x, Mod.Type t)
    let let_sig x s = Let (x, Mod.Sig s)

    let rec ok ctx t =
      match t with
      | Let (x, m) ->
        let lx = F.Expr.Name.to_label x in
        let (Target.Asig.Exists (alphas, csig), e) = Mod.ok ctx m in
        ( alphas
        , F.Label.Map.singleton lx csig
        , fun acc -> F.Expr.Let (x, e, acc) )
      | Nil ->
        ( []
        , F.Label.Map.empty
        , fun acc -> acc )
      | Cat (b1, b2) ->
        let (alphas1, map1, b1) = ok ctx b1 in
        let ctx =
          List.fold ~init:ctx alphas1
            ~f:(fun ctx (a, k) -> Ctx.add_ty ctx a k)
        in
        let ctx =
          Map.fold ~init:ctx map1
            ~f:(fun ~key:lx ~data:csig ctx ->
              let x = F.Expr.Name.of_label lx in
              Ctx.add_tm ctx x csig)
        in
        let (alphas2, map2, b2) = ok ctx b2 in
        ( alphas1 @ alphas2
        , Map.merge map1 map2 (fun ~key:_ ys ->
            match ys with
            | `Left x -> Some x
            | `Right x -> Some x
            | `Both (_, x) -> Some x)
        , fun acc -> b1 (b2 acc) )
      | Include m ->
        let (Target.Asig.Exists (alphas, csig), e) = Mod.ok ctx m in
        begin
          match csig with
          | Target.Csig.Struct map ->
            let (m, intro_m) =
              match e with
              | F.Expr.Name m -> (m, (fun x -> x))
              | _ ->
                let m = F.Expr.Name.raw "mod" in
                (m, fun body -> F.Expr.Let (m, e, body))
            in
            ( alphas
            , map
            , fun acc -> intro_m begin
                Map.fold map ~init:acc
                  ~f:(fun ~key:lx ~data:_ acc ->
                    let x = F.Expr.Name.of_label lx in
                    (* CR: only introduce the binding if x is free in acc *)
                    F.Expr.Let (x, F.Expr.Dot (F.Expr.Name m, lx), acc))
              end)
          | _ -> failwith "included non-struct"
        end
      | Local (b1, b2) -> (* derived form *)
        ok ctx (Include (Mod.Let (b1, Mod.Struct b2)))

  end

end

