open Std_internal

open Systemf

module rec Csig : sig
  type t =
    | Val of Type.t
    | Type of Type.t * Kind.t
    | Sig of Asig.t
    | Struct of t Label.Map.t
    | Fun of (Type.Name.t * Kind.t) list * t * Asig.t

  val to_f : t -> Type.t

  val fvs : t -> Type.Name.Set.t

  val subst : t -> Type.Name.t * Type.t -> t

  val swap : Type.Name.t * Type.Name.t -> t -> t

  val sub : Type.Context.t -> t -> t -> [`Coerce of Term.t -> Term.t]

  val matches :
    Type.Context.t -> t -> Asig.t ->
      (Type.t * Kind.t) list * [`Coerce of Term.t -> Term.t]

end = struct
  type t =
    | Val of Type.t
    | Type of Type.t * Kind.t
    | Sig of Asig.t
    | Struct of t Label.Map.t
    | Fun of (Type.Name.t * Kind.t) list * t * Asig.t

  let rec to_f = function
    | Val t -> t
    | Type (t, k) ->
      let p = Type.Name.next (Type.Name.raw "p") ~not_in:(Type.fvs t) in
      Type.Forall
        ( p
        , Kind.Arr (k, Kind.Star)
        , Type.Arr
            ( Type.App (Type.Name p, t)
            , Type.App (Type.Name p, t)))
    | Sig asig ->
      let asig = Asig.to_f asig in
      Type.Arr (asig, asig)
    | Struct map ->
      Type.Record (Label.Map.map map ~f:to_f)
    | Fun (aks, csig, asig) ->
      List.fold_right aks
        ~f:(fun (a, k) acc -> Type.Exists (a, k, acc))
        ~init:(Type.Arr (to_f csig, Asig.to_f asig))

    let fvs t = Type.fvs (to_f t)

    let swap p =
      let rec swap = function
        | Val t -> Val (Type.swap p t)
        | Type (t, k) -> Type (Type.swap p t, k)
        | Sig asig -> Sig (Asig.swap p asig)
        | Struct map -> Struct (Label.Map.map map ~f:swap)
        | Fun (aks, csig, asig) ->
          Fun
            ( List.map aks ~f:(fun (a, k) -> (Type.Name.swap p a, k))
            , swap csig
            , Asig.swap p asig )
      in
      swap

  let subst t sub =
    let rec subst = function
      | Val t -> Val (Type.subst t sub)
      | Type (t, k) -> Type (Type.subst t sub, k)
      | Sig asig -> Sig (Asig.subst asig sub)
      | Struct map -> Struct (Label.Map.map map ~f:subst)
      | Fun (aks, csig, asig) ->
        let (aks, csig, asig, _) =
          let (+) = Set.union in
          let (-) = Set.remove in
          let rec freshen aks csig asig fvs =
            match aks with
            | [] -> ([], csig, asig, fvs)
            | (a, k) :: aks ->
              let (aks, csig, asig, fvs) = freshen aks csig asig fvs in
              let a' = Type.Name.next a ~not_in:fvs in
              ( (a', k) :: aks
              , swap (a, a') csig
              , Asig.swap (a, a') asig
              , Set.add (fvs - a) a' )
          in
          freshen aks csig asig begin
            fvs csig + Asig.fvs asig + Type.fvs (snd sub)
          end
        in
        Fun (aks, subst csig, Asig.subst asig sub)
    in
    subst t

  let rec sub ctx csig1 csig2 =
    match (csig1, csig2) with
    | (Csig.Val t1, Csig.Val t2) ->
      Systemf.subtype ctx ~src:t1 ~dst:t2
    | (Csig.Type (t1, _), Csig.Type (t2, _)) ->
      if not (Systemf.Type.equal t1 t2) then failwith "type mismatch";
      `Coerce (fun x -> x)
    | (Csig.Sig s1, Csig.Sig s2) ->
      let `Coerce _ = Asig.sub ctx s1 s2 in
      let `Coerce _ = Asig.sub ctx s2 s1 in
      `Coerce (fun _ -> Term.sig_mod (Asig.to_f s2))
    | (Csig.Struct map1, Csig.Struct map2) ->
      let map =
        Label.Map.merge map1 map2 (fun ~key:_ cs ->
          match cs with
          | `Left _ -> None
          | `Right _ -> failwith "missing binding"
          | `Both (csig1, csig2) ->
            let `Coerce f = sub ctx csig1 csig2 in
            Some f)
      in
      `Coerce (fun e ->
        Term.Record
          (Map.mapi map
            ~f:(fun ~key:lx ~data:f -> f (Term.Dot (e, lx)))))
    | (Fun (aks1, csig1, asig1), Fun (aks2, csig2, asig2)) ->
      let ctx =
        (* CR: this must be wrong -- we should be freshening as
           we unbind or something *)
        List.fold aks2 ~init:ctx ~f:(fun ctx (a, k) ->
          Type.Context.add ctx a k)
      in
      let (tks, `Coerce fdom) =
        matches ctx csig2 (Asig.Exists (aks1, csig1))
      in
      let `Coerce frng =
        let sub =
          List.map ~f:(fun ((a, _k), (t, _k')) -> (a, t))
            (List.zip_exn aks1 tks)
        in
        Asig.sub ctx (List.fold ~f:Asig.subst ~init:asig1 sub) asig2
      in
      `Coerce (fun f ->
        List.fold_right ~f:(fun (a, k) e -> Term.Ty_fun (a, k, e)) aks2
          ~init:begin
            let x = Term.Name.dummy in
            Term.Fun (x, Csig.to_f csig2,
              frng begin
                Term.App
                  ( List.fold tks ~init:f
                      ~f:(fun e (t, _k) -> Term.Ty_app (e, t))
                  , fdom (Term.Name x) )
              end
            )
          end
      )
    | _ -> failwith "signature mismatch"

  and matches _ _ = failwith "UNIMPLEMENTED Csig.matches"

end

and Asig : sig

  type t = Exists of (Type.Name.t * Kind.t) list * Csig.t

  val fvs : t -> Type.Name.Set.t

  val swap : Type.Name.t * Type.Name.t -> t -> t

  val to_f : t -> Type.t

  val sub : Type.Context.t -> t -> t -> [`Coerce of Term.t -> Term.t]

  val subst : t -> Type.Name.t * Type.t -> t

end = struct

  type t = Exists of (Type.Name.t * Kind.t) list * Csig.t

  let rec to_f = function
    | Exists (aks, csig) ->
      List.fold_right aks
        ~f:(fun (a, k) acc -> Type.Exists (a, k, acc))
        ~init:(Csig.to_f csig)

  let fvs t = Type.fvs (to_f t)

  let swap p (Exists (aks, csig)) =
    Exists
      ( List.map aks ~f:(fun (a, k) -> (Type.Name.swap p a, k))
      , Csig.swap p csig )

  let subst (Exists (aks, csig)) sub =
    let (aks, csig, _) =
      let (+) = Set.union in
      let (-) = Set.remove in
      let rec freshen aks csig fvs =
        match aks with
        | [] -> ([], csig, fvs)
        | (a, k) :: aks ->
          let (aks, csig, fvs) = freshen aks csig fvs in
          let a' = Type.Name.next a ~not_in:fvs in
          ( (a', k) :: aks
          , Csig.swap (a, a') csig
          , Set.add (fvs - a) a' )
      in
      freshen aks csig (Csig.fvs csig + Type.fvs (snd sub))
    in
    Exists (aks, Csig.subst csig sub)

  let sub ctx asig1 asig2 =
    match (asig1, asig2) with
    | (Exists ([], csig1), Exists ([], csig2)) ->
      Csig.sub ctx csig1 csig2
    | (Exists (aks1, csig1), (Exists (aks2, csig2) as asig2)) ->
      let (ts, `Coerce f) = Csig.matches ctx csig1 asig2 in
      `Coerce (fun x ->
        let y = Term.Name.dummy in
        let taks =
          List.map ~f:(fun ((a, k), (t, _)) -> (t, a, k))
            (List.zip_exn aks2 ts)
        in
        Term.unpack (List.map ~f:fst aks1) y x
          (Term.pack taks (f (Term.Name y)) (Csig.to_f csig2)))

end

module Context = struct
  type t = {
    ty_ctx : Type.Context.t;
    tm_ctx : Csig.t Term.Name.Map.t;
  }

  let empty = {
    ty_ctx = Type.Context.empty;
    tm_ctx = Term.Name.Map.empty;
  }

  let add_ty g a k = {g with ty_ctx = Type.Context.add g.ty_ctx a k}
  let find_ty g a = Type.Context.find g.ty_ctx a

  let add_tm g x t = {g with tm_ctx = Map.add g.tm_ctx x t}
  let find_tm g x = Map.find g.tm_ctx x

  let ty_ctx t = t.ty_ctx
end


