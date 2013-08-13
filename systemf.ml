open Std_internal
open Unbound

module Kind = struct

  type t = Star | Arr of t * t with sexp

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"Systemf.Kind.t"
      module Label = struct
        type 'a t =
        | Star : unit t
        | Arr : (o * o) t
        let name_of : type a. a t -> string = function
          | Star -> "star"
          | Arr -> "arr"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Star -> Type.Rep.Unit
          | Arr -> Type.Rep.(Pair (orep, orep))
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

module Ty = struct

  type t =
  | Name   of t Name.t
  | Arr    of t * t
  | Record of t Label.Map.t
  | Forall of (t Name.t * Kind.t Embed.t, t) Bind.t
  | Exists of (t Name.t * Kind.t Embed.t, t) Bind.t
  | Fun    of (t Name.t * Kind.t Embed.t, t) Bind.t
  | App    of t * t
  with sexp

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"Systemf.Type.t"
      module Label = struct
        type 'a t =
        | Name   : o Name.t t
        | Arr    : (o * o) t
        | Record : o Label.Map.t t
        | Forall : (o Name.t * Kind.t Embed.t, o) Bind.t t
        | Exists : (o Name.t * Kind.t Embed.t, o) Bind.t t
        | Fun    : (o Name.t * Kind.t Embed.t, o) Bind.t t
        | App    : (o * o) t
        let name_of : type a. a t -> string = function
          | Name   -> "name"
          | Arr    -> "arr"
          | Record -> "record"
          | Forall -> "forall"
          | Exists -> "exists"
          | Fun    -> "fun"
          | App    -> "app"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Name   -> Name.type_rep orep
          | Arr    -> Type.Rep.(Pair (orep, orep))
          | Record -> Label.map_type_rep type_rep
          | Forall -> Bind.type_rep (Type.Rep.Pair (Name.type_rep orep, Embed.type_rep Kind.type_rep)) orep
          | Exists -> Bind.type_rep (Type.Rep.Pair (Name.type_rep orep, Embed.type_rep Kind.type_rep)) orep
          | Fun    -> Bind.type_rep (Type.Rep.Pair (Name.type_rep orep, Embed.type_rep Kind.type_rep)) orep
          | App    -> Type.Rep.(Pair (orep, orep))
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

  module X = Name

  module Name = Name.Make (struct
    type nonrec a = t
    let name = "Type.Name"
    let type_rep = type_rep
  end)

  let unbind b =
    let ((x, k), t) =
      Bind.unbind (Type.Rep.Pair (Name.type_rep, Embed.type_rep Kind.type_rep)) type_rep b
    in
    (x, ((k : Kind.t Embed.t) :> Kind.t), t)

  let bind (x, k, t) = Bind.create (x, Embed.create k) t

  let forall xkt = Forall (bind xkt)
  let exists xkt = Exists (bind xkt)
  let fun_   xkt = Fun    (bind xkt)

  let fvs t =
    Free_vars.Term.fv type_rep t
    |! Set.to_list
    |! List.filter_map ~f:Name.of_univ
    |! Name.Set.of_list

  let swap (a, b) t =
    let a = Name.to_univ a in
    let b = Name.to_univ b in
    Swap.swap type_rep (X.Univ.Perm.swap a b) t

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

module Tm = struct

  type t =
  | Name of t Name.t
  | Fun of (t Name.t * Ty.t Embed.t, t) Bind.t
  | App of t * t
  | Record of t Label.Map.t
  | Dot of t * Label.t
  | Tyfun of (Ty.Name.t * Kind.t Embed.t, t) Bind.t
  | Tyapp of t * Ty.t
  | Pack of Ty.t * t * (Ty.Name.t, Ty.t) Bind.t
  | Unpack of (Ty.Name.t * t Name.t * t Embed.t, t) Bind.t
  | Let of (t Name.t * t Embed.t, t) Bind.t
  with sexp

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"Systemf.Term.t"
      module Label = struct
        type 'a t =
        | Name   : o Name.t t
        | Fun    : (o Name.t * Ty.t Embed.t, o) Bind.t t
        | App    : (o * o) t
        | Record : o Label.Map.t t
        | Dot    : (o * Label.t) t
        | Tyfun  : (Ty.Name.t * Kind.t Embed.t, o) Bind.t t
        | Tyapp  : (o * Ty.t) t
        | Pack   : (Ty.t * o * (Ty.Name.t, Ty.t) Bind.t) t
        | Unpack : (Ty.Name.t * o Name.t * o Embed.t, o) Bind.t t
        | Let    : (o Name.t * o Embed.t, o) Bind.t t
        let name_of : type a. a t -> string = function
          | Name   -> "name"
          | Fun    -> "fun"
          | App    -> "app"
          | Record -> "record"
          | Dot    -> "dot"
          | Tyfun  -> "tyfun"
          | Tyapp  -> "tyapp"
          | Pack   -> "pack"
          | Unpack -> "unpack"
          | Let    -> "let"
                      let type_of : type a. a t -> a Type.Rep.t = function
                        | Name   -> Name.type_rep orep
                        | Fun    -> Bind.type_rep (Type.Rep.Pair (Name.type_rep orep, Embed.type_rep Ty.type_rep)) orep
                        | App    -> Type.Rep.Pair (orep, orep)
                        | Record -> Label.map_type_rep orep
                        | Dot    -> Type.Rep.Pair (orep, Label.type_rep)
                        | Tyfun -> Bind.type_rep (Type.Rep.Pair (Ty.Name.type_rep, Embed.type_rep Kind.type_rep)) orep
                        | Tyapp  -> Type.Rep.Pair (orep, Ty.type_rep)
                        | Pack   -> Type.Rep.Triple (Ty.type_rep, orep, Bind.type_rep Ty.Name.type_rep Ty.type_rep)
                        | Unpack -> Bind.type_rep (Type.Rep.Triple (Ty.Name.type_rep, Name.type_rep orep, Embed.type_rep orep)) orep
                        | Let    -> Bind.type_rep (Type.Rep.Pair (Name.type_rep orep, Embed.type_rep orep)) orep
                                    type univ = Label : 'a t -> univ
                                    let all = [ Label Name; Label Fun; Label App; Label Record; Label Dot;
                                                Label Tyfun; Label Tyapp; Label Pack; Label Unpack; Label Let ]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Name a         -> Tagged (Label.Name,   a)
        | Fun a          -> Tagged (Label.Fun,    a)
        | App (a, b)     -> Tagged (Label.App,    (a, b))
        | Record a       -> Tagged (Label.Record, a)
        | Dot (a, b)     -> Tagged (Label.Dot,    (a, b))
        | Tyfun a        -> Tagged (Label.Tyfun,  a)
        | Tyapp (a, b)   -> Tagged (Label.Tyapp,  (a, b))
        | Pack (a, b, c) -> Tagged (Label.Pack,   (a, b, c))
        | Unpack a       -> Tagged (Label.Unpack, a)
        | Let a          -> Tagged (Label.Let,    a)
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Name,   a)         -> Name a
        | (Label.Fun,    a)         -> Fun a
        | (Label.App,    (a, b))    -> App (a, b)
        | (Label.Record, a)         -> Record a
        | (Label.Dot,    (a, b))    -> Dot (a, b)
        | (Label.Tyfun,  a)         -> Tyfun a
        | (Label.Tyapp,  (a, b))    -> Tyapp (a, b)
        | (Label.Pack,   (a, b, c)) -> Pack (a, b, c)
        | (Label.Unpack, a)         -> Unpack a
        | (Label.Let,    a)         -> Let a
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = t)

  let mk_fun x ty body =
    Fun (Bind.create (x, Embed.create ty) body)

  let un_fun b =
    let ((x, ty), body) =
      Bind.unbind
        (Type.Rep.Pair
           (Name.type_rep type_rep, Embed.type_rep Ty.type_rep))
        type_rep
        b
    in
    (x, ((ty : Ty.t Embed.t) :> Ty.t), body)
  ;;

  let mk_tyfun a k body =
    Tyfun (Bind.create (a, Embed.create k) body)

  let un_tyfun b =
    let ((a, k), body) =
      Bind.unbind
        (Type.Rep.Pair
           ( Ty.Name.type_rep,
             Embed.type_rep Kind.type_rep ))
        type_rep
        b
    in
    (a, ((k : Kind.t Embed.t) :> Kind.t), body)
  ;;

  let mk_pack ty tm (a, body) =
    Pack (ty, tm, Bind.create a body)

  let un_pack b =
    Bind.unbind Ty.Name.type_rep Ty.type_rep b

  let mk_unpack a x tm body =
    Unpack (Bind.create (a, x, Embed.create tm) body)

  let un_unpack b : Ty.Name.t * t Name.t * t * t =
    let ((x, a, tm), body) =
      Bind.unbind
        (Type.Rep.Triple
           ( Ty.Name.type_rep,
             Name.type_rep type_rep,
             Embed.type_rep type_rep
           ))
        type_rep
        b
    in
    (x, a, ((tm : t Embed.t) :> t), body)

  let mk_let x tm body = Let (Bind.create (x, Embed.create tm) body)

  let un_let b =
    let ((x, e), body) =
      Bind.unbind
        (Type.Rep.Pair
           ( Name.type_rep type_rep,
             Embed.type_rep type_rep ))
        type_rep
        b
    in
    (x, ((e : t Embed.t) :> t), body)

  module Name = struct
    include Name.Make (struct
      type nonrec a = t
      let name = "Type.Name"
      let type_rep = type_rep
    end)
    let to_label t = to_univ t |> Name.Univ.to_string |> Label.of_string
    let of_label l = Label.to_string l |> raw
  end

  module Context : sig
    type t
    val empty   : t
    val add_tm  : t -> Name.t -> Ty.t -> t
    val find_tm : t -> Name.t -> Ty.t option
    val add_ty  : t -> Ty.Name.t -> Kind.t -> t
    val ty_ctx  : t -> Ty.Context.t
  end = struct

    type t = {
      ty_ctx : Ty.Context.t;
      tm_ctx : Ty.t Name.Map.t;
    }

    let empty = {
      ty_ctx = Ty.Context.empty;
      tm_ctx = Name.Map.empty;
    }

    let add_ty g a k = {g with ty_ctx = Ty.Context.add g.ty_ctx a k}

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
    | Fun b ->
      let (x, t, e) = un_fun b in
      Ty.check_star (Context.ty_ctx ctx) t;
      Ty.Arr (t, ok (Context.add_tm ctx x t) e)
    | App (efun, earg) ->
      let targ = ok ctx earg in
      (match ok ctx efun with
      | Ty.Arr (tdom, trng) ->
        if Ty.equal targ tdom then trng else
          failwith "type mismatch"
      | _ -> failwith "expected arrow type")
    | Record xes -> Ty.Record (Label.Map.map xes ~f:(fun e -> ok ctx e))
    | Dot (e, x) ->
      (match ok ctx e with
      | Ty.Record map ->
        (match Label.Map.find map x with
        | Some t -> t
        | None -> failwith "undefined field")
      | _ -> failwith "expected record type")
    | Tyfun b ->
      let (a, k, e) = un_tyfun b in
      let (a, e) =
        let a' = assert false in
        let e' = swap_ty (a, a') e in
        (a', e')
      in
      Ty.forall (a, k, ok (Context.add_ty ctx a k) e)
    | Tyapp (e, targ) ->
      let karg = Ty.ok (Context.ty_ctx ctx) targ in
      (match ok ctx e with
      | Ty.Forall bnd ->
        let (a, kdom, trng) = Ty.unbind bnd in
        if Kind.equal kdom karg then Ty.subst trng (a, targ) else
          failwith "kind mismatch"
      | _ -> failwith "expected forall type")
    | Pack (tsub, e, exty) ->
      let (a, tintf) = un_pack exty in
      let ty_ctx = Context.ty_ctx ctx in
      let k = Ty.ok ty_ctx tsub in
      let t = Ty.exists (a, k, tintf) in
      Ty.check_star ty_ctx t;
      let tfull = ok ctx e in
      if Ty.equal tfull (Ty.subst tintf (a, tsub)) then t else
        failwith "existential type mismatch"
    | Unpack b ->
      let (a1, x, edef, ebody) = un_unpack b in
      (match ok ctx edef with
      | Ty.Exists bnd ->
        let (a2, k, tbody) = Ty.unbind bnd in
        let (a, ebody, tbody) =
          let a = assert false in
          let ebody = swap_ty   (a, a1) ebody in
          let tbody = Ty.swap (a, a2) tbody in
          (a, ebody, tbody)
        in
        let ctx = Context.add_ty ctx a k in
        let ctx = Context.add_tm ctx x tbody in
        let t = ok ctx ebody in
        if Set.mem (Ty.fvs t) a
        then failwith "existential type escaping its scope"
        else t
      | _ -> failwith "unpacking a non-existential")
    | Let b ->
      let (x, edef, ebody) = un_let b in
      let t = ok ctx edef in
      let ctx = Context.add_tm ctx x t in
      ok ctx ebody

  let type_mod t k =
    let a = assert false in
    let x = assert false (* Name.dummy *) in
    mk_tyfun a (Kind.Arr (k, Kind.Star))
      (mk_fun x (Ty.App (Ty.Name a, t)) (Name x))

  let sig_mod t =
    let x = assert false (* Name.dummy *) in
    mk_fun x t (Name x)

  let pack taks e t =
    fst
      (List.fold_right taks ~init:(e, t) ~f:(fun (t, a, k) (e, tann) ->
        (mk_pack t e (a, tann), Ty.exists (a, k, tann))))

  let rec unpack alphas x edef ebody =
    match alphas with
    | [] -> mk_let x edef ebody
    | [a] -> mk_unpack a x edef ebody
    | a :: rest ->
      mk_unpack a x edef (unpack rest x (Name x) ebody)

end

let rec subtype _ctx ~src ~dst =
  match (src, dst) with
  | (Ty.Name a, Ty.Name b) when Ty.Name.equal a b -> `Coerce (fun x -> x)
  | _ -> failwith "UNIMPLEMENTED: Systemf.subtype"

