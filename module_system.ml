open Std_internal

(* CR: always normalize signatures by using set-binders *)

module F = F

module Ctx = Target.Context

module Source (Base : sig
  module Kind : sig
    type t with sexp
    val ok : Ctx.t -> t -> F.Kind.t
  end
  module Ty : sig
    type 'a t with sexp
    type 'a check = Ctx.t -> 'a -> F.Ty.t * F.Kind.t
    val ok : 'a check -> 'a t check
  end
  module Tm : sig
    type ('a, 'b) t with sexp
    type 'b check = Ctx.t -> 'b -> F.Tm.t * F.Ty.t
    val ok : 'a Ty.check -> 'b check -> ('a, 'b) t check
  end
end) = struct

  module Kind = Base.Kind

  module rec Path : sig
    type t = Mod.t with sexp
    val ok : Ctx.t -> t -> F.Tm.t * Target.Csig.t
  end = struct
    type t = Mod.t with sexp
    let ok ctx m =
      let (Target.Asig.Exists b, e) = Mod.ok ctx m in
      let (alphas, csig) = Target.Asig.un_exists b in
      if
        let fvs = F.Ty.fvs (Target.Csig.to_f csig) in
        List.exists alphas ~f:(fun (a, _k) -> Set.mem fvs a)
      then
        failwith "path projection lets type variable escape"
      else begin
        let x = F.Tm.Name.raw "path.project" in
        ( List.fold alphas ~init:e ~f:(fun acc (a, _) ->
          F.Tm.mk_unpack a x acc (F.Tm.Name x))
            , csig )
      end
  end

  and Ty : sig
    type t =
    | Wrap of t Base.Ty.t
    | Path of Path.t
    | Let of Bnd.t * t
    with sexp
    val ok : t Base.Ty.check
  end = struct

    type t =
    | Wrap of t Base.Ty.t
    | Path of Path.t
    | Let of Bnd.t * t
    with sexp

    let rec ok ctx t =
      match t with
      | Wrap typ -> Base.Ty.ok ok ctx typ
      | Path p ->
        let (_, csig) = Path.ok ctx p in
        (match csig with
        | Target.Csig.Type (t, k) -> (t, k)
        | _ -> failwith "non-type module embedded in type expression")
      | Let (b, t) -> (* derived form *)
        let x = F.Tm.Name.raw "local.type" in
        let lx = F.Tm.Name.to_label x in
        ok ctx
          (Path (Mod.Dot (Mod.Struct (Bnd.Cat (b, Bnd.let_typ x t)), lx)))

  end

  and Tm : sig
    type t =
    | Wrap of (Ty.t, t) Base.Tm.t
    | Path of Path.t
    | Let of Bnd.t * t
    with sexp
    val ok : t Base.Tm.check
  end = struct

    type t =
    | Wrap of (Ty.t, t) Base.Tm.t
    | Path of Path.t
    | Let of Bnd.t * t
    with sexp

    let rec ok ctx e =
      match e with
      | Wrap exp -> Base.Tm.ok Ty.ok ok ctx exp
      | Path p ->
        let (e, csig) = Path.ok ctx p in
        (match csig with
        | Target.Csig.Val t -> (e, t)
        | _ -> failwith "non-term module embedded in term expression")
      | Let (b, e) -> (* derived form *)
        let x = F.Tm.Name.raw "local.expr" in
        let lx = F.Tm.Name.to_label x in
        ok ctx (Path (Mod.Dot (Mod.Struct (Bnd.Cat (b, Bnd.let_val x e)), lx)))

  end

  and Sig : sig
    type t =
    | Path of Path.t
    | Val of Ty.t
    | Type of Ty.t
    | Abstype of Kind.t
    | Sig of Sig.t
    | Struct of Decl.t
    | Fun of F.Tm.Name.t * t * t
    | Where of t * F.Label.t list * Ty.t
    | Let of Bnd.t * t
    with sexp
    val ok : Ctx.t -> t -> Target.Asig.t
  end = struct

    type t =
    | Path of Path.t
    | Val of Ty.t
    | Type of Ty.t
    | Abstype of Kind.t
    | Sig of Sig.t
    | Struct of Decl.t
    | Fun of F.Tm.Name.t * t * t
    | Where of t * F.Label.t list * Ty.t
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
        let (t, k) = Ty.ok ctx t in
        begin
          match k with
          | F.Kind.Star -> ()
          | _ -> failwith "improper type"
        end;
        Target.Asig.mk_exists [] (Target.Csig.Val t)
      | Type t ->
        let (t, k) = Ty.ok ctx t in
        Target.Asig.mk_exists [] (Target.Csig.Type (t, k))
      | Abstype k ->
        let k = Kind.ok ctx k in
        let a = assert false (* F.Ty.Name.dummy *) in
        let t = F.Ty.Name a in
        Target.Asig.mk_exists [(a, k)] (Target.Csig.Type (t, k))
      | Sig s ->
        let asig = Sig.ok ctx s in
        Target.Asig.mk_exists [] (Target.Csig.Sig asig)
      | Struct d ->
        let (alphas, map) = Decl.ok ctx d in
        Target.Asig.mk_exists alphas (Target.Csig.Struct map)
      | Fun (x, s, s') ->
        let Target.Asig.Exists b = Sig.ok ctx s in
        let (aks, csig) = Target.Asig.un_exists b in
        let ctx =
          List.fold aks ~init:ctx ~f:(fun ctx (a, k) ->
            Target.Context.add_ty ctx a k)
        in
        let ctx = Target.Context.add_tm ctx x csig in
        let asig = Sig.ok ctx s' in
        Target.Asig.mk_exists []
          (Target.Csig.mk_fun aks csig asig)
      | Where (s, path, t) ->
        let Target.Asig.Exists b = Sig.ok ctx s in
        let (aks, csig_orig) = Target.Asig.un_exists b in
        let (t, k) = Ty.ok ctx t in
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
              | F.Ty.Name a ->
                let alphas =
                  List.filter aks ~f:(fun (a', _) ->
                    not (F.Ty.Name.equal a a'))
                in
                Target.Asig.mk_exists alphas
                  (Target.Csig.subst csig_orig (a, t))
              | _ -> failwith "where constraint mentions non-abstract type")
            | _ -> failwith "bad path in where constraint (type 3)")
        in
        loop csig_orig path

      | Let (b, e) -> (* derived form *)
        let x = F.Tm.Name.raw "local.sig" in
        let lx = F.Tm.Name.to_label x in
        ok ctx
          (Path (Mod.Dot (Mod.Struct (Bnd.Cat (b, Bnd.let_sig x e)), lx)))
  end

  and Decl : sig

    type t =
      | Decl of F.Tm.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    with sexp

    val ok :
      Ctx.t
      -> t
      -> (F.Ty.Name.t * F.Kind.t) list
       * Target.Csig.t F.Label.Map.t

  end = struct

    type t =
      | Decl of F.Tm.Name.t * Sig.t
      | Nil
      | Cat of t * t
      | Include of Sig.t
      | Local of Bnd.t * t
    with sexp

    let rec ok ctx = function
      | Decl (x, s) ->
        let Target.Asig.Exists b = Sig.ok ctx s in
        let (aks, csig) = Target.Asig.un_exists b in
        let lx = F.Tm.Name.to_label x in
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
              let x = F.Tm.Name.of_label lx in
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
        let Target.Asig.Exists b = Sig.ok ctx s in
        let (aks, csig) = Target.Asig.un_exists b in
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
      | Name of F.Tm.Name.t
      | Val of Tm.t
      | Type of Ty.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * F.Label.t
      | Fun of F.Tm.Name.t * Sig.t * t
      | App of t * t
      | Seal of t * Sig.t
      | Let of Bnd.t * t
    with sexp
    val ok : Ctx.t -> t -> Target.Asig.t * F.Tm.t
  end = struct

    type t =
      | Name of F.Tm.Name.t
      | Val of Tm.t
      | Type of Ty.t
      | Sig of Sig.t
      | Struct of Bnd.t
      | Dot of t * F.Label.t
      | Fun of F.Tm.Name.t * Sig.t * t
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
          failwithf "unbound var %s" (F.Tm.Name.to_string x) ()
        | Some csig -> (Target.Asig.mk_exists [] csig, F.Tm.Name x))
      | Val e ->
        let (e, t) = Tm.ok ctx e in
        ( Target.Asig.mk_exists [] (Target.Csig.Val t)
        , e )
      | Type t ->
        let (t, k) = Ty.ok ctx t in
        ( Target.Asig.mk_exists [] (Target.Csig.Type (t, k))
        , F.Tm.type_mod t k )
      | Sig s ->
        let asig = Sig.ok ctx s in
        ( Target.Asig.mk_exists [] (Target.Csig.Sig asig)
        , F.Tm.sig_mod (Target.Asig.to_f asig) )
      | Struct bnd ->
        let (aks, map, bnd) = Bnd.ok ctx bnd in
        let asig =
          Target.Asig.mk_exists aks (Target.Csig.Struct map)
        in
        ( asig
        , bnd (F.Tm.Record
            (Map.mapi map ~f:(fun ~key:x ~data:_ ->
              F.Tm.Name (F.Tm.Name.of_label x)))) )
      | Dot (m, x) ->
        let (Target.Asig.Exists b, e) = ok ctx m in
        let (alphas, csig) = Target.Asig.un_exists b in
        begin
          match csig with
          | Target.Csig.Struct map ->
            (match F.Label.Map.find map x with
            | None -> failwith "unknown field"
            | Some xsig ->
              (Target.Asig.mk_exists alphas xsig, F.Tm.Dot (e, x)))
          | _ -> failwith "field projection from non-struct"
        end
      | Fun (x, s, m) ->
        let Target.Asig.Exists b = Sig.ok ctx s in
        let (aks, csig) = Target.Asig.un_exists b in
        let ctx =
          List.fold aks ~init:ctx ~f:(fun ctx (a, k) ->
            Target.Context.add_ty ctx a k)
        in
        let ctx = Target.Context.add_tm ctx x csig in
        let (asig, e) = ok ctx m in
        ( Target.Asig.mk_exists []
            (Target.Csig.mk_fun aks csig asig)
        , List.fold_right aks
            ~f:(fun (a, k) acc -> F.Tm.mk_tyfun a k acc)
            ~init:(F.Tm.mk_fun x (Target.Csig.to_f csig) e) )
      | App (Name x1, Name x2) ->
        let (aks, csig', asig) =
          match lookup ctx x1 with
          | Target.Csig.Fun b -> Target.Csig.un_fun b
          | _ -> failwith "applied non-function"
        in
        let csig = lookup ctx x2 in
        let (tks, `Coerce f) =
          Target.Csig.matches (Ctx.ty_ctx ctx) csig
            (Target.Asig.mk_exists aks csig')
        in
        let subs =
          List.zip_exn (List.map ~f:fst aks) (List.map ~f:fst tks)
        in
        (* CR: does this require simultaneous substitution? *)
        ( List.fold ~f:Target.Asig.subst ~init:asig subs
        , F.Tm.App
            ( List.fold ~init:(F.Tm.Name x1) tks
                ~f:(fun acc (t, _k) -> F.Tm.Tyapp (acc, t))
            , f (F.Tm.Name x2)) )
      | Seal (Name x, s) ->
        let csig = lookup ctx x in
        let asig = Sig.ok ctx s in
        let (tks, `Coerce f) =
          Target.Csig.matches (Ctx.ty_ctx ctx) csig asig
        in
        (match asig with
        | Target.Asig.Exists b ->
          let (alphas, csig') = Target.Asig.un_exists b in
          let taks =
            List.map (List.zip_exn alphas tks)
              ~f:(fun ((a, k), (t, _k)) -> (t, a, k))
          in
          (* CR: something is wrong, we never unpack! *)
          ( asig
          , F.Tm.pack taks (f (F.Tm.Name x)) (Target.Csig.to_f csig') ))
      | Let (b, m) -> (* derived form *)
        let x = assert false (* F.Tm.Name.dummy *) in
        let lx = F.Tm.Name.to_label x in
        let m =  Dot (Struct (Bnd.Cat (b, Bnd.Let (x, m))), lx) in
        ok ctx m
      | App (m1, m2) -> (* derived form *)
        let x1 = F.Tm.Name.raw "fun" in
        let x2 = F.Tm.Name.raw "arg" in
        let m =
          Let
            ( Bnd.Cat (Bnd.Let (x1, m1), Bnd.Let (x2, m2))
            , App (Name x1, Name x2) )
        in
        ok ctx m
      | Seal (m, s) -> (* derived form *)
        let x = F.Tm.Name.raw "conc" in
        let m = Let (Bnd.Let (x, m), Seal (Name x, s)) in
        ok ctx m
  end

  and Bnd : sig

    type t =
      | Let of F.Tm.Name.t * Mod.t
      | Nil
      | Cat of t * t
      | Include of Mod.t
      | Local of t * t
    with sexp

    val let_val : F.Tm.Name.t -> Tm.t -> t
    val let_typ : F.Tm.Name.t -> Ty.t -> t
    val let_sig : F.Tm.Name.t -> Sig.t -> t

    val ok :
      Ctx.t
      -> t
      -> (F.Ty.Name.t * F.Kind.t) list
       * Target.Csig.t F.Label.Map.t
       * (F.Tm.t -> F.Tm.t)

  end = struct

    type t =
      | Let of F.Tm.Name.t * Mod.t
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
        let lx = F.Tm.Name.to_label x in
        let (Target.Asig.Exists b, e) = Mod.ok ctx m in
        let (alphas, csig) = Target.Asig.un_exists b in
        ( alphas
        , F.Label.Map.singleton lx csig
        , fun acc -> F.Tm.mk_let x e acc )
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
              let x = F.Tm.Name.of_label lx in
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
        let (Target.Asig.Exists b, e) = Mod.ok ctx m in
        let (alphas, csig) = Target.Asig.un_exists b in
        begin
          match csig with
          | Target.Csig.Struct map ->
            let (m, intro_m) =
              match e with
              | F.Tm.Name m -> (m, (fun x -> x))
              | _ ->
                let m = F.Tm.Name.raw "mod" in
                (m, fun body -> F.Tm.mk_let m e body)
            in
            ( alphas
            , map
            , fun acc -> intro_m begin
                Map.fold map ~init:acc
                  ~f:(fun ~key:lx ~data:_ acc ->
                    let x = F.Tm.Name.of_label lx in
                    (* CR: only introduce the binding if x is free in acc *)
                    F.Tm.mk_let x (F.Tm.Dot (F.Tm.Name m, lx)) acc)
              end)
          | _ -> failwith "included non-struct"
        end
      | Local (b1, b2) -> (* derived form *)
        ok ctx (Include (Mod.Let (b1, Mod.Struct b2)))

  end

end

