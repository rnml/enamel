open Std_internal

open Systemf

module rec Csig : sig
  type t =
    | Val of Ty.t
    | Type of Ty.t * Kind.t
    | Sig of Asig.t
    | Struct of t Label.Map.t
    | Fun of (Ty.Name.t * Kind.t) list * t * Asig.t
  with sexp

  val to_f : t -> Ty.t

  val fvs : t -> Ty.Name.Set.t

  val subst : t -> Ty.Name.t * Ty.t -> t

  val swap : Ty.Name.t * Ty.Name.t -> t -> t

  val sub : Ty.Context.t -> t -> t -> [`Coerce of Tm.t -> Tm.t]

  val matches :
    Ty.Context.t -> t -> Asig.t ->
      (Ty.t * Kind.t) list * [`Coerce of Tm.t -> Tm.t]

end = struct
  type t =
  | Val of Ty.t
  | Type of Ty.t * Kind.t
  | Sig of Asig.t
  | Struct of t Label.Map.t
  | Fun of (Ty.Name.t * Kind.t) list * t * Asig.t
  with sexp

  let rec to_f = function
    | Val t -> t
    | Type (t, k) ->
      let p = assert false in
      Ty.forall
        ( p
        , Kind.Arr (k, Kind.Star)
        , Ty.Arr
          ( Ty.App (Ty.Name p, t)
          , Ty.App (Ty.Name p, t)))
    | Sig asig ->
      let asig = Asig.to_f asig in
      Ty.Arr (asig, asig)
    | Struct map ->
      Ty.Record (Label.Map.map map ~f:to_f)
    | Fun (aks, csig, asig) ->
      List.fold_right aks
        ~f:(fun (a, k) acc -> Ty.exists (a, k, acc))
        ~init:(Ty.Arr (to_f csig, Asig.to_f asig))

  let fvs _ = assert false
  let swap _ = assert false
  let subst _ _ = assert false

  let rec sub ctx csig1 csig2 =
    match (csig1, csig2) with
    | (Csig.Val t1, Csig.Val t2) ->
      Systemf.subtype ctx ~src:t1 ~dst:t2
    | (Csig.Type (t1, _), Csig.Type (t2, _)) ->
      if not (Systemf.Ty.equal t1 t2) then failwith "type mismatch";
      `Coerce (fun x -> x)
    | (Csig.Sig s1, Csig.Sig s2) ->
      let `Coerce _ = Asig.sub ctx s1 s2 in
      let `Coerce _ = Asig.sub ctx s2 s1 in
      `Coerce (fun _ -> Tm.sig_mod (Asig.to_f s2))
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
        Tm.Record
          (Map.mapi map
             ~f:(fun ~key:lx ~data:f -> f (Tm.Dot (e, lx)))))
    | (Fun (aks1, csig1, asig1), Fun (aks2, csig2, asig2)) ->
      let ctx =
        (* CR: this must be wrong -- we should be freshening as
           we unbind or something *)
        List.fold aks2 ~init:ctx ~f:(fun ctx (a, k) ->
          Ty.Context.add ctx a k)
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
        List.fold_right aks2
          ~f:(fun (a, k) e -> Tm.mk_tyfun a k e)
          ~init:begin
            let x = assert false (* Tm.Name.dummy *) in
            Tm.mk_fun
              x
              (Csig.to_f csig2)
              (frng begin
                Tm.App
                  ( List.fold tks ~init:f
                      ~f:(fun e (t, _k) -> Tm.Tyapp (e, t))
                      , fdom (Tm.Name x) )
              end)
          end)
    | _ -> failwith "signature mismatch"

  and matches ctx csig (Asig.Exists (alphas, csig')) =
    let tks =
      List.map alphas ~f:(fun (alpha, kind) ->
        let rec lookup csig csig' =
          match (csig, csig') with
          | (Type (tau, k), Type (Ty.Name alpha', k')) when Ty.Name.equal alpha alpha' ->
            if Kind.equal k k' && Kind.equal k kind then
              Some (tau, k)
            else
              failwith "kind mismatch"
          | (Struct m1, Struct m2) ->
            Map.merge m1 m2 ~f:(fun ~key:_ data ->
              match data with
              | `Left _ | `Right _ -> None
              | `Both (a, b) -> Some (a, b)
            )
            |> Map.data
            |> List.find_map ~f:(fun (csig, csig') -> lookup csig csig')
          | _ -> None
        in
        match lookup csig csig' with
        | None ->
          (* let dump num csig =
           *   prerr_endline (Int.to_string num ^ ": " ^ Sexp.to_string_hum (Csig.sexp_of_t csig))
           * in
           * dump 1 csig;
           * dump 2 csig'; *)
          failwithf "missing typevar" ()
        | Some x -> x)
    in
    let atks = List.zip_exn alphas tks in
    let csig' =
      List.fold atks ~init:csig' ~f:(fun acc ((alpha, _k1), (tau, _k2)) -> subst acc (alpha, tau))
    in
    let `Coerce f = sub ctx csig csig' in
    (tks, `Coerce f)

end

and Asig : sig

  type t = Exists of (Ty.Name.t * Kind.t) list * Csig.t with sexp

  val fvs : t -> Ty.Name.Set.t

  val swap : Ty.Name.t * Ty.Name.t -> t -> t

  val to_f : t -> Ty.t

  val sub : Ty.Context.t -> t -> t -> [`Coerce of Tm.t -> Tm.t]

  val subst : t -> Ty.Name.t * Ty.t -> t

end = struct

  type t = Exists of (Ty.Name.t * Kind.t) list * Csig.t with sexp

  let rec to_f = function
    | Exists (aks, csig) ->
      List.fold_right aks
        ~f:(fun (a, k) acc -> Ty.exists (a, k, acc))
        ~init:(Csig.to_f csig)

  let fvs _ = assert false
  let swap _ _ = assert false

  let subst (Exists (aks, csig)) sub =
    let (aks, csig, _) =
      let (+) = Set.union in
      let (-) = Set.remove in
      let rec freshen aks csig fvs =
        match aks with
        | [] -> ([], csig, fvs)
        | (a, k) :: aks ->
          let (aks, csig, fvs) = freshen aks csig fvs in
          let a' = assert false (* Ty.Name.next a ~not_in:fvs *) in
          ( (a', k) :: aks
          , Csig.swap (a, a') csig
          , Set.add (fvs - a) a' )
      in
      freshen aks csig (Csig.fvs csig + Ty.fvs (snd sub))
    in
    Exists (aks, Csig.subst csig sub)

  let sub ctx asig1 asig2 =
    match (asig1, asig2) with
    | (Exists ([], csig1), Exists ([], csig2)) ->
      Csig.sub ctx csig1 csig2
    | (Exists (aks1, csig1), (Exists (aks2, csig2) as asig2)) ->
      let (ts, `Coerce f) = Csig.matches ctx csig1 asig2 in
      `Coerce (fun x ->
        let y = assert false (* Tm.Name.dummy *) in
        let taks =
          List.map ~f:(fun ((a, k), (t, _)) -> (t, a, k))
            (List.zip_exn aks2 ts)
        in
        Tm.unpack (List.map ~f:fst aks1) y x
          (Tm.pack taks (f (Tm.Name y)) (Csig.to_f csig2)))

end

module Context = struct
  type t = {
    ty_ctx : Ty.Context.t;
    tm_ctx : Csig.t Tm.Name.Map.t;
  } with sexp

  let empty = {
    ty_ctx = Ty.Context.empty;
    tm_ctx = Tm.Name.Map.empty;
  }

  let add_ty g a k = {g with ty_ctx = Ty.Context.add g.ty_ctx a k}
  let find_ty g a = Ty.Context.find g.ty_ctx a

  let add_tm g x t = {g with tm_ctx = Map.add g.tm_ctx x t}
  let find_tm g x = Map.find g.tm_ctx x

  let ty_ctx t = t.ty_ctx
end


