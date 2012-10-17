open Std_internal

module Kind = struct
  type t = Star | Arr of t * t
  let rec equal k k' =
    match (k, k') with
    | (Star, Star) -> true
    | (Arr (k1, k2), Arr (k1', k2')) -> equal k1 k1' && equal k2 k2'
    | _ -> false
end

module Label : Name.S = Name.Make (struct end)

module Type = struct

  module Name : Name.S = Name.Make (struct end)

  module Names = struct
    include Name.Set
    let nil = empty
    let (+) = union
    let (-) = remove
  end

  type t =
    | Name of Name.t
    | Arr of t * t
    | Record of t Label.Map.t
    | Forall of Name.t * Kind.t * t
    | Exists of Name.t * Kind.t * t
    | Fun of Name.t * Kind.t * t
    | App of t * t

  let rec fvs =
    let open Names in
    function
      | Name x -> Name.Set.singleton x
      | Record xts ->
        Map.fold xts ~init:nil
          ~f:(fun ~key:_ ~data:t acc -> acc + fvs t)
      | App (t1, t2)
      | Arr (t1, t2) -> fvs t1 + fvs t2
      | Forall (x, _, t)
      | Exists (x, _, t)
      | Fun (x, _, t) -> fvs t - x

  let swap p =
    let rec swap = function
      | Name a -> Name (Name.swap p a)
      | Record m -> Record (Label.Map.map m ~f:swap)
      | App (t1, t2) -> App (swap t1, swap t2)
      | Arr (t1, t2) -> Arr (swap t1, swap t2)
      | Forall (a, k, t) -> Forall (Name.swap p a, k, swap t)
      | Exists (a, k, t) -> Exists (Name.swap p a, k, swap t)
      | Fun    (a, k, t) -> Fun    (Name.swap p a, k, swap t)
    in
    swap

  let subst t sub =
    let sub_fvs = fvs (snd sub) in
    let freshen (a, t) =
      let a' = Name.next a ~not_in:(Name.Set.union sub_fvs (fvs t)) in
      let t' = swap (a, a') t in
      (a', t')
    in
    let rec subst = function
      | Name a -> if Name.equal a (fst sub) then snd sub else Name a
      | Record m -> Record (Label.Map.map m ~f:subst)
      | App (t1, t2) -> App (subst t1, subst t2)
      | Arr (t1, t2) -> Arr (subst t1, subst t2)
      | Forall (a, k, t) -> let (a, t) = freshen (a, t) in Forall (a, k, subst t)
      | Exists (a, k, t) -> let (a, t) = freshen (a, t) in Exists (a, k, subst t)
      | Fun    (a, k, t) -> let (a, t) = freshen (a, t) in Fun    (a, k, subst t)
    in
    subst t

  let whnf t =
    let rec loop t args =
      match t with
      | App (tf, tx) -> loop tf (tx :: args)
      | Fun (a, k, tbody) ->
        (match args with
        | [] -> Fun (a, k, tbody)
        | arg :: args -> loop (subst tbody (a, arg)) args)
      | head ->
        List.fold args ~init:head ~f:(fun tf tx -> App (tf, tx))
    in
    loop t []

  let rec equal t t' =
    let fresh_eq (a1, t1) (a2, t2) =
      let a = Name.next a1 ~not_in:(Name.Set.union (fvs t1) (fvs t2)) in
      let t1 = swap (a, a1) t1 in
      let t2 = swap (a, a2) t2 in
      equal t1 t2
    in
    match (whnf t, whnf t') with
    | (Name a, Name a') -> Name.equal a a'
    | (Record m, Record m') -> Label.Map.equal equal m m'
    | (App (ta, tb), App (ta', tb'))
    | (Arr (ta, tb), Arr (ta', tb')) -> equal ta ta' && equal tb tb'
    | (Forall (a, k, t), Forall (a', k', t'))
    | (Exists (a, k, t), Exists (a', k', t'))
    | (Fun    (a, k, t), Fun    (a', k', t')) -> Kind.equal k k' && fresh_eq (a, t) (a', t')
    | (_, (Name _ | Record _ | App _ | Arr _ | Forall _ | Exists _ | Fun _)) -> false

  module Context : sig
    type t
    val empty : t
    val add   : t -> Name.t -> Kind.t -> t
    val find  : t -> Name.t -> Kind.t option
    val domain : t -> Name.Set.t
  end = struct
    type t = Kind.t Name.Map.t
    let empty = Name.Map.empty
    let add t a k =
      assert (not (Map.mem t a));
      Map.add t ~key:a ~data:k
    let find = Map.find
    let domain t = Name.Set.of_list (Map.keys t)
  end

  let rec ok ctx = function
    | Name a ->
      (match Context.find ctx a with
      | None -> failwith "unbound type variable"
      | Some k -> k)
    | Arr (t1, t2) ->
      check_star ctx t1;
      check_star ctx t2;
      Kind.Star
    | Record entries ->
      Label.Map.iter entries ~f:(fun ~key:_ ~data:t -> check_star ctx t);
      Kind.Star
    | Forall (a, k, t)
    | Exists (a, k, t) -> check_star (Context.add ctx a k) t; Kind.Star
    | Fun (a, k, t) -> Kind.Arr (k, ok (Context.add ctx a k) t)
    | App (tfun, targ) ->
      let karg = ok ctx targ in
      (match ok ctx tfun with
      | Kind.Arr (kdom, krng) ->
        if Kind.equal karg kdom then krng else failwith "kind mismatch"
      | _ -> failwith "expected arrow kind")

  and check_star ctx t =
    match ok ctx t with
    | Kind.Star -> ()
    | _ -> failwith "expected kind star"

end

module Term = struct

  module Name : sig
    include Name.S
    val to_label : t -> Label.t
    val of_label : Label.t -> t
  end = struct
    include Name.Make (struct end)
    let to_label t = Label.of_name (to_name t)
    let of_label l = of_name (Label.to_name l)
  end

  type t =
    | Name of Name.t
    | Fun of Name.t * Type.t * t
    | App of t * t
    | Record of t Label.Map.t
    | Dot of t * Label.t
    | Ty_fun of Type.Name.t * Kind.t * t
    | Ty_app of t * Type.t
    | Pack of (* pack <ty, tm> : exists a. ty *)
        Type.t * t * Type.Name.t * Type.t
    | Unpack of Type.Name.t * Name.t * t * t
    | Let of Name.t * t * t

  module Context : sig
    type t
    val empty   : t
    val add_tm  : t -> Name.t -> Type.t -> t
    val find_tm : t -> Name.t -> Type.t option
    val add_ty  : t -> Type.Name.t -> Kind.t -> t
    val ty_ctx  : t -> Type.Context.t
  end = struct

    type t = {
      ty_ctx : Type.Context.t;
      tm_ctx : Type.t Name.Map.t;
    }

    let empty = {
      ty_ctx = Type.Context.empty;
      tm_ctx = Name.Map.empty;
    }

    let add_ty g a k = {g with ty_ctx = Type.Context.add g.ty_ctx a k}

    let ty_ctx g = g.ty_ctx

    let add_tm g x s =
      {g with tm_ctx = Map.add g.tm_ctx ~key:x ~data:s}

    let find_tm g x = Map.find g.tm_ctx x
  end

  let rec ftvs =
    let open Type.Names in
    function
    | Name _ -> nil
    | Fun (_, t, e) -> Type.fvs t + ftvs e
    | App (efun, earg) -> ftvs efun + ftvs earg
    | Record xes ->
      Map.fold xes ~init:nil
        ~f:(fun ~key:_ ~data:e acc -> acc + ftvs e)
    | Dot (e, _) -> ftvs e
    | Ty_fun (a, _, e) -> ftvs e - a
    | Ty_app (e, targ) -> ftvs e + Type.fvs targ
    | Pack (tsub, e, a, tbody) ->
      Type.fvs tsub + ftvs e + (Type.fvs tbody - a)
    | Unpack (a, _, edef, ebody) -> ftvs edef + (ftvs ebody - a)
    | Let (_, e1, e2) -> ftvs e1 + ftvs e2

  let swap_ty p =
    let tswap t = Type.swap p t in
    let aswap a = Type.Name.swap p a in
    let rec swap = function
      | Name x -> Name x
      | Fun (x,       t,      e) ->
        Fun (x, tswap t, swap e)
      | App (     efun,      earg) ->
        App (swap efun, swap earg)
      | Record xes -> Record (Label.Map.map xes ~f:swap)
      | Dot (     e, x) ->
        Dot (swap e, x)
      | Ty_fun (      a, k,      e) ->
        Ty_fun (aswap a, k, swap e)
      | Ty_app (     e,       t) ->
        Ty_app (swap e, tswap t)
      | Pack (      tsub,      e,       a,       tbody) ->
        Pack (tswap tsub, swap e, aswap a, tswap tbody)
      | Unpack (      a, x,      edef,      ebody) ->
        Unpack (aswap a, x, swap edef, swap ebody)
      | Let (x,      edef,      ebody) ->
        Let (x, swap edef, swap ebody)
    in
    swap

  let rec ok ctx = function
    | Name x ->
      (match Context.find_tm ctx x with
      | None -> failwith "unbound term variable"
      | Some t -> t)
    | Fun (x, t, e) ->
      Type.check_star (Context.ty_ctx ctx) t;
      Type.Arr (t, ok (Context.add_tm ctx x t) e)
    | App (efun, earg) ->
      let targ = ok ctx earg in
      (match ok ctx efun with
      | Type.Arr (tdom, trng) ->
        if Type.equal targ tdom then trng else
          failwith "type mismatch"
      | _ -> failwith "expected arrow type")
    | Record xes -> Type.Record (Label.Map.map xes ~f:(fun e -> ok ctx e))
    | Dot (e, x) ->
      (match ok ctx e with
      | Type.Record map ->
        (match Label.Map.find map x with
        | Some t -> t
        | None -> failwith "undefined field")
      | _ -> failwith "expected record type")
    | Ty_fun (a, k, e) ->
      let (a, e) =
        let a' =
          Type.Name.next a ~not_in:begin
            let open Type.Names in
              Type.Context.domain (Context.ty_ctx ctx)
              + ftvs e
          end
        in
        let e' = swap_ty (a, a') e in
        (a', e')
      in
      Type.Forall (a, k, ok (Context.add_ty ctx a k) e)
    | Ty_app (e, targ) ->
      let karg = Type.ok (Context.ty_ctx ctx) targ in
      (match ok ctx e with
      | Type.Forall (a, kdom, trng) ->
        if Kind.equal kdom karg then Type.subst trng (a, targ) else
          failwith "kind mismatch"
      | _ -> failwith "expected forall type")
    | Pack (tsub, e, a, tintf) ->
      let ty_ctx = Context.ty_ctx ctx in
      let k = Type.ok ty_ctx tsub in
      let t = Type.Exists (a, k, tintf) in
      Type.check_star ty_ctx t;
      let tfull = ok ctx e in
      if Type.equal tfull (Type.subst tintf (a, tsub)) then t else
      failwith "existential type mismatch"
    | Unpack (a1, x, edef, ebody) ->
      (match ok ctx edef with
      | Type.Exists (a2, k, tbody) ->
        let (a, ebody, tbody) =
          let a =
            Type.Name.next a1 ~not_in:begin
              let open Type.Names in
              Type.Context.domain (Context.ty_ctx ctx)
                + ftvs ebody
                + Type.fvs tbody
            end
          in
          let ebody = swap_ty   (a, a1) ebody in
          let tbody = Type.swap (a, a2) tbody in
          (a, ebody, tbody)
        in
        let ctx = Context.add_ty ctx a k in
        let ctx = Context.add_tm ctx x tbody in
        let t = ok ctx ebody in
        if Set.mem (Type.fvs t) a
        then failwith "existential type escaping its scope"
        else t
      | _ -> failwith "unpacking a non-existential")
    | Let (x, edef, ebody) ->
      let t = ok ctx edef in
      let ctx = Context.add_tm ctx x t in
      ok ctx ebody

  let type_mod t k =
    let a = Type.Name.next (Type.Name.raw "p") ~not_in:(Type.fvs t) in
    let x = Name.dummy in
    Ty_fun (a, Kind.Arr (k, Kind.Star),
      Fun (x, Type.App (Type.Name a, t), Name x))

  let sig_mod t =
    let x = Name.dummy in
    Fun (x, t, Name x)

  let pack taks e t =
    fst
      (List.fold_right taks ~init:(e, t) ~f:(fun (t, a, k) (e, tann) ->
        (Pack (t, e, a, tann), Type.Exists (a, k, tann))))

  let rec unpack alphas x edef ebody =
    match alphas with
    | [] -> Let (x, edef, ebody)
    | [a] -> Unpack (a, x, edef, ebody)
    | a :: rest -> Unpack (a, x, edef, unpack rest x (Name x) ebody)

end

let rec subtype _ctx ~src:_ ~dst:_ =
  failwith "UNIMPLEMENTED: Systemf.subtype"

