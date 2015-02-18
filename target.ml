open Std_internal

open Unbound
open F

module Args = struct
  type t = (Type.Name.t * Kind.t Embed.t) list with sexp

  let type_rep =
    Type.Rep.List
      (Type.Rep.Pair
         (Type.Name.type_rep, Embed.type_rep Kind.type_rep))

  let bind tks body =
    let tks = List.map tks ~f:(fun (t, k) -> (t, Embed.create k)) in
    Bind.create tks body

  let unbind body_type_rep b =
    let (tks, body) = Bind.unbind type_rep body_type_rep b in
    (List.map tks ~f:(fun (t, k) ->
       (t, ((k : Kind.t Embed.t) :> Kind.t))), body)

end

module rec Csig : sig
  type t =
    | Val of Type.t
    | Type of Type.t * Kind.t
    | Sig of Asig.t
    | Struct of t Label.Map.t
    | Fun of (Args.t, t * Asig.t) Bind.t
  with sexp

  val type_rep : t Type.Rep.t Lazy.t

  val mk_fun : (Type.Name.t * Kind.t) list -> t -> Asig.t -> t

  val un_fun
    :  (Args.t, t * Asig.t) Bind.t
    -> (Type.Name.t * Kind.t) list * t * Asig.t

  val to_f : t -> Type.t

  val subst : t -> Type.Name.t * Type.t -> t

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
    | Fun of (Args.t, t * Asig.t) Bind.t
  with sexp

  let rec type_rep = lazy begin
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"F.Kind.t"
      module Label = struct
        type 'a t =
          | Val : Type.t t
          | Type : (Type.t * Kind.t) t
          | Sig : Asig.t t
          | Struct : o Label.Map.t t
          | Fun : (Args.t, o * Asig.t) Bind.t t
        let name_of : type a. a t -> string = function
          | Val    -> "val"
          | Type   -> "type"
          | Sig    -> "sig"
          | Struct -> "struct"
          | Fun    -> "fun"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Val    -> Type.type_rep
          | Type   -> Type.Rep.(Pair (Type.type_rep, Kind.type_rep))
          | Sig    -> Lazy.force Asig.type_rep
          | Struct -> Label.map_type_rep (Lazy.force orep)
          | Fun    -> Bind.type_rep Args.type_rep (Type.Rep.Pair (Lazy.force orep, Lazy.force Asig.type_rep))
        type univ = Label : 'a t -> univ
        let all = [
          Label Val;
          Label Type;
          Label Sig;
          Label Struct;
          Label Fun;
        ]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Val a       -> Tagged (Label.Val, a)
        | Type (a, b) -> Tagged (Label.Type, (a, b))
        | Sig a       -> Tagged (Label.Sig, a)
        | Struct a    -> Tagged (Label.Struct, a)
        | Fun a       -> Tagged (Label.Fun, a)
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Val, a)       -> Val a
        | (Label.Type, (a, b)) -> Type (a, b)
        | (Label.Sig, a)       -> Sig a
        | (Label.Struct, a)    -> Struct a
        | (Label.Fun, a)       -> Fun a
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = t)
  end

  let mk_fun tks csig asig = Fun (Args.bind tks (csig, asig))

  let un_fun b =
    let (aks, (csig, asig)) =
      Args.unbind
        (Type.Rep.Pair
           (Lazy.force type_rep, Lazy.force Asig.type_rep))
        b
    in
    (aks, csig, asig)

  let rec to_f = function
    | Val t -> t
    | Type (t, k) ->
      let p = Type.Name.create "p" in
      Type.forall
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
    | Fun b ->
      let (aks, csig, asig) = un_fun b in
      List.fold_right aks
        ~f:(fun (a, k) acc -> Type.exists (a, k, acc))
        ~init:(Type.Arr (to_f csig, Asig.to_f asig))

  let rec subst t sub =
    match t with
    | Val a -> Val (Type.subst a sub)
    | Type (a, k) -> Type (Type.subst a sub, k)
    | Sig asig -> Sig (Asig.subst asig sub)
    | Struct m -> Struct (Map.map m ~f:(fun t -> subst t sub))
    | Fun bnd ->
      let (aks, csig, asig) = un_fun bnd in
      let csig = subst csig sub in
      let asig = Asig.subst asig sub in
      mk_fun aks csig asig

  let rec sub ctx csig1 csig2 =
    match (csig1, csig2) with
    | (Csig.Val t1, Csig.Val t2) ->
      F.subtype ctx ~src:t1 ~dst:t2
    | (Csig.Type (t1, _), Csig.Type (t2, _)) ->
      if not (F.Type.equal t1 t2) then failwith "type mismatch";
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
    | (Fun b1 , Fun b2) ->
      let (aks1, csig1, asig1) = un_fun b1 in
      let (aks2, csig2, asig2) = un_fun b2 in
      let ctx =
        (* CR: this must be wrong -- we should be freshening as
           we unbind or something *)
        List.fold aks2 ~init:ctx ~f:(fun ctx (a, k) ->
          Type.Context.add ctx a k)
      in
      let (tks, `Coerce fdom) =
        matches ctx csig2 (Asig.mk_exists aks1 csig1)
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
          ~f:(fun (a, k) e -> Term.mk_tyfun a k e)
          ~init:begin
            let x = Term.Name.create "x" in
            Term.mk_fun x (Csig.to_f csig2) (frng begin
              Term.App
                ( List.fold tks ~init:f
                    ~f:(fun e (t, _k) -> Term.Tyapp (e, t))
                , fdom (Term.Name x) )
            end)
          end)
    | _ -> failwith "signature mismatch"

  and matches ctx csig (Asig.Exists b) =
    let (alphas, csig') = Asig.un_exists b in
    let tks =
      List.map alphas ~f:(fun (alpha, kind) ->
        let rec lookup csig csig' =
          match (csig, csig') with
          | (Type (tau, k), Type (Type.Name alpha', k'))
            when Type.Name.equal alpha alpha'
            ->
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

  type t = Exists of (Args.t, Csig.t) Bind.t with sexp

  val type_rep : t Type.Rep.t Lazy.t

  val mk_exists : (Type.Name.t * Kind.t) list -> Csig.t -> t

  val un_exists
    :  (Args.t, Csig.t) Bind.t
    -> (Type.Name.t * Kind.t) list * Csig.t

  val to_f : t -> Type.t

  val sub : Type.Context.t -> t -> t -> [`Coerce of Term.t -> Term.t]

  val subst : t -> Type.Name.t * Type.t -> t

end = struct

  type t = Exists of (Args.t, Csig.t) Bind.t with sexp

  let rec type_rep = lazy begin
    Type.Rep.Variant (module struct
      type o = t
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"F.Kind.t"
      module Label = struct
        type 'a t =
          | Exists : (Args.t, Csig.t) Bind.t t
        let name_of : type a. a t -> string = function
          | Exists -> "exists"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Exists -> Bind.type_rep Args.type_rep (Lazy.force Csig.type_rep)
        type univ = Label : 'a t -> univ
        let all = [Label Exists]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Exists a -> Tagged (Label.Exists, a)
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Exists, a) -> Exists a
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = t)
  end

  let mk_exists tks csig = Exists (Args.bind tks csig)

  let un_exists b = Args.unbind (Lazy.force Csig.type_rep) b

  let rec to_f = function
    | Exists b ->
      let (aks, csig) = un_exists b in
      List.fold_right aks
        ~f:(fun (a, k) acc -> Type.exists (a, k, acc))
        ~init:(Csig.to_f csig)

  let subst (Exists b) sub =
    let (tks, csig) = un_exists b in
    mk_exists tks (Csig.subst csig sub)

  let sub ctx (Exists b1) ((Exists b2) as asig2) =
    match (un_exists b1, un_exists b2) with
    | (([], csig1), ([], csig2)) ->
      Csig.sub ctx csig1 csig2
    | ((aks1, csig1), (aks2, csig2)) ->
      let (ts, `Coerce f) = Csig.matches ctx csig1 asig2 in
      `Coerce (fun x ->
        let y = Term.Name.create "y" in
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
  } with sexp

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
