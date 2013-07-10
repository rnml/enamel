open Std_internal

module Systemf = struct

  module Kind = struct

    type t =
    | Star
    | Arr of t * t
    with sexp

    let rec type_rep : t Type.Rep.t =
      Type.Rep.Variant (module struct
        type outer_t = t
        type t = outer_t
        let name : t Type.Name.t = Type.Name.create ~name:"Systemf.Kind.t"
        module Label = struct
          type 'a t =
          | Star : unit t
          | Arr : (outer_t * outer_t) t
          let name_of : type a. a t -> string = function
            | Star -> "star"
            | Arr -> "arr"
          let type_of : type a. a t -> a Type.Rep.t = function
            | Star -> Type.Rep.Unit
            | Arr  -> Type.Rep.Pair (type_rep, type_rep)
          type univ = Label : 'a t -> univ
          let all = [Label Star; Label Arr]
        end
        type 'a tag = 'a Label.t
        type rep = Tagged : 'a tag * 'a -> rep
        let project = function
          | Star       -> Tagged (Label.Star, ())
          | Arr (a, b) -> Tagged (Label.Arr, (a, b))
        let put (type a) (tag : a tag) (arg : a) : t =
          match (tag, arg) with
          | (Label.Star, ())    -> Star
          | (Label.Arr, (a, b)) -> Arr (a, b)
        let inject = fun (Tagged (tag, arg)) -> put tag arg
      end : Type.Rep.Variant.T with type t = t)
  end

end

module Label : Name.S = Name.Make (struct end)

  (*
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
  with sexp

  let rec type_rep : t Type.Rep.t =
    Type.Rep.Variant (module struct
      type outer_t = t
      type t = outer_t
      let name : t Type.Name.t = Type.Name.create ~name:"Systemf.Kind.t"
      module Label = struct
        type 'a t =
        | Star : unit t
        | Arr : (outer_t * outer_t) t

  | Name : Name.t t
  | Arr of outer_t * outer_t
  | Record of outer_t Label.Map.t
  | Forall of Name.t * Kind.t * outer_t
  | Exists of Name.t * Kind.t * outer_t
  | Fun of Name.t * Kind.t * outer_t
  | App of outer_t * outer_t
        let name_of : type a. a t -> string = function
          | Star -> "star"
          | Arr -> "arr"
                      let type_of : type a. a t -> a Type.Rep.t = function
                        | Star -> Type.Rep.Unit
                        | Arr  -> Type.Rep.Pair (type_rep, type_rep)
                                    type univ = Label : 'a t -> univ
                                    let all = [Label Star; Label Arr]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Star       -> Tagged (Label.Star, ())
        | Arr (a, b) -> Tagged (Label.Arr, (a, b))
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Star, ())    -> Star
        | (Label.Arr, (a, b)) -> Arr (a, b)
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = t)

end

  *)
(*


  module Context : sig
    type t with sexp
    val empty : t
    val add   : t -> Name.t -> Kind.t -> t
    val find  : t -> Name.t -> Kind.t option
    val domain : t -> Name.Set.t
  end = struct
    type t = Kind.t Name.Map.t with sexp
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

module Expr = struct

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
  with sexp

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

let rec subtype _ctx ~src ~dst =
  match (src, dst) with
  | (Type.Name a, Type.Name b) when Type.Name.equal a b -> `Coerce (fun x -> x)
  | _ -> failwith "UNIMPLEMENTED: Systemf.subtype"

    *)
