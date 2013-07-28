open Std_internal
open Unbound

module Kind = struct

  type t = Star | Arr of t * t with sexp

  let rec type_rep =
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
          | Arr -> Type.Rep.(Pair (type_rep, type_rep))
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

  let rec equal k k' =
    match (k, k') with
    | (Star, Star) -> true
    | (Arr (k1, k2), Arr (k1', k2')) -> equal k1 k1' && equal k2 k2'
    | _ -> false

end

module Label = struct
  type t = string
  include Identifiable.Make (struct
    include String
    let module_name = "Label"
  end)
  let type_rep = Type.Rep.String
  module Map_type_rep = Type.Name.Make1 (struct type 'a t = 'a Map.t end)
  let map_type_rep_name = Map_type_rep.lookup
  let map_type_rep x =
    let name = map_type_rep_name (Type.Rep.id x) in
    List.iter [
      (Free_vars.Term.register, Free_vars.Term.fold);
      (Free_vars.Pat.register,  Free_vars.Pat.fold);
    ]
      ~f:(fun (register, fold) ->
        register name (fun acc map -> List.fold (Map.data map) ~init:acc ~f:(fold x))) ;
    Swap.register name (fun perm map -> Map.map map ~f:(fun d -> Swap.swap x perm d));
    Type.Rep.Abstract name
end

module Type = struct

  type t =
    | Name   of t Name.t
    | Arr    of t * t
    | Record of t Label.Map.t
    | Forall of (t Name.t * Kind.t Embed.t, t) Bind.t
    | Exists of (t Name.t * Kind.t Embed.t, t) Bind.t
    | Fun    of (t Name.t * Kind.t Embed.t, t) Bind.t
    | App    of t * t
  with sexp

  module Name = Name.Make (struct
    type nonrec a = t
    let name = "Type.Name"
  end)

  let rec type_rep =
    Type.Rep.Variant (module struct
      type outer_t = t
      type t = outer_t
      let name : t Type.Name.t = Type.Name.create ~name:"Systemf.Type.t"
      module Label = struct
        type 'a t =
        | Name : Name.t t
        | Arr : (outer_t * outer_t) t
        | Record : outer_t Label.Map.t t
        | Forall : (Name.t * Kind.t Embed.t, outer_t) Bind.t t
        | Exists : (Name.t * Kind.t Embed.t, outer_t) Bind.t t
        | Fun    : (Name.t * Kind.t Embed.t, outer_t) Bind.t t
        | App : (outer_t * outer_t) t
        let name_of : type a. a t -> string = function
          | Name -> "name"
          | Arr -> "arr"
          | Record -> "record"
          | Forall -> "forall"
          | Exists -> "exists"
          | Fun    -> "fun"
          | App -> "app"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Name   -> Name.type_rep
          | Arr    -> Type.Rep.(Pair (type_rep, type_rep))
          | Record -> Label.map_type_rep type_rep
          | Forall -> Bind.type_rep (Type.Rep.Pair (Name.type_rep, Embed.type_rep Kind.type_rep)) type_rep
          | Exists -> Bind.type_rep (Type.Rep.Pair (Name.type_rep, Embed.type_rep Kind.type_rep)) type_rep
          | Fun    -> Bind.type_rep (Type.Rep.Pair (Name.type_rep, Embed.type_rep Kind.type_rep)) type_rep
          | App    -> Type.Rep.(Pair (type_rep, type_rep))
        type univ = Label : 'a t -> univ
        let all = [Label Name; Label Arr; Label App]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Name x     -> Tagged (Label.Name, x)
        | Arr (a, b) -> Tagged (Label.Arr, (a, b))
        | Record m   -> Tagged (Label.Record, m)
        | Forall x   -> Tagged (Label.Forall, x)
        | Exists x   -> Tagged (Label.Exists, x)
        | Fun x      -> Tagged (Label.Fun, x)
        | App (a, b) -> Tagged (Label.App, (a, b))
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Name, x)     -> Name x
        | (Label.Arr, (a, b)) -> Arr (a, b)
        | (Label.Record, m)    -> Record m
        | (Label.Forall, x)   -> Forall x
        | (Label.Exists, x)   -> Exists x
        | (Label.Fun, x)      -> Fun x
        | (Label.App, (a, b)) -> App (a, b)
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = t)

  let unbind b =
    let ((x, k), t) =
      Bind.unbind (Type.Rep.Pair (Name.type_rep, Embed.type_rep Kind.type_rep)) type_rep b
    in
    (x, ((k : Kind.t Embed.t) :> Kind.t), t)

  let bind (x, k, t) = Bind.create (x, Embed.create k) t

  let forall xkt = Forall (bind xkt)
  let exists xkt = Exists (bind xkt)
  let fun_   xkt = Fun    (bind xkt)

  let fvs _ = assert false
  let swap _ = assert false
  let subst _ _ = assert false
  let whnf _ = assert false
  let equal _ _ = assert false

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
    | Forall b | Exists b ->
      let (a, k, t) = unbind b in
      check_star (Context.add ctx a k) t; Kind.Star
    | Fun b ->
      let (a, k, t) = unbind b in
      Kind.Arr (k, ok (Context.add ctx a k) t)
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

  type t =
    | Name of t Name.t
    | Fun of t Name.t * Type.t * t
    | App of t * t
    | Record of t Label.Map.t
    | Dot of t * Label.t
    | Ty_fun of Type.Name.t * Kind.t * t
    | Ty_app of t * Type.t
    | Pack of (* pack <ty, tm> : exists a. ty *)
        Type.t * t * Type.Name.t * Type.t
    | Unpack of Type.Name.t * t Name.t * t * t
    | Let of t Name.t * t * t
  with sexp

  module Name = struct
    include Name.Make (struct
      type nonrec a = t
      let name = "Type.Name"
    end)
    let to_label t = to_univ t |> Name.Univ.to_string |> Label.of_string
    let of_label l = Label.to_string l |> raw
  end

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

  let rec ftvs _ = assert false

  let swap_ty _ = assert false

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
        let a' = assert false in
        let e' = swap_ty (a, a') e in
        (a', e')
      in
      Type.forall (a, k, ok (Context.add_ty ctx a k) e)
    | Ty_app (e, targ) ->
      let karg = Type.ok (Context.ty_ctx ctx) targ in
      (match ok ctx e with
      | Type.Forall bnd ->
        let (a, kdom, trng) = Type.unbind bnd in
        if Kind.equal kdom karg then Type.subst trng (a, targ) else
          failwith "kind mismatch"
      | _ -> failwith "expected forall type")
    | Pack (tsub, e, a, tintf) ->
      let ty_ctx = Context.ty_ctx ctx in
      let k = Type.ok ty_ctx tsub in
      let t = Type.exists (a, k, tintf) in
      Type.check_star ty_ctx t;
      let tfull = ok ctx e in
      if Type.equal tfull (Type.subst tintf (a, tsub)) then t else
      failwith "existential type mismatch"
    | Unpack (a1, x, edef, ebody) ->
      (match ok ctx edef with
      | Type.Exists bnd ->
        let (a2, k, tbody) = Type.unbind bnd in
        let (a, ebody, tbody) =
          let a = assert false in
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
    let a = assert false in
    let x = assert false (* Name.dummy *) in
    Ty_fun (a, Kind.Arr (k, Kind.Star),
      Fun (x, Type.App (Type.Name a, t), Name x))

  let sig_mod t =
    let x = assert false (* Name.dummy *) in
    Fun (x, t, Name x)

  let pack taks e t =
    fst
      (List.fold_right taks ~init:(e, t) ~f:(fun (t, a, k) (e, tann) ->
        (Pack (t, e, a, tann), Type.exists (a, k, tann))))

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

