open Std_internal
open Unbound

open Or_error.Monad_infix

let fold_right_non_empty (x, xs) ~f =
  let g x acc =
    match acc with
    | None -> x
    | Some y -> f x y
  in
  g x (List.fold_right xs ~init:None ~f:(fun x acc -> Some (g x acc)))

module Kind = struct

  type t = Star | Arr of t * t

  let rec sexp_of_t = function
    | Star -> Sexp.Atom "*"
    | Arr (a, b) ->
      let rec unravel = function
        | Arr (u, v) -> u :: unravel v
        | other -> [other]
      in
      Sexp.List (Sexp.Atom "Fun" :: List.map ~f:sexp_of_t (a :: unravel b))

  let rec t_of_sexp = function
    | Sexp.Atom "*" -> Star
    | Sexp.List (Sexp.Atom "Fun" :: a :: b) ->
      let a = t_of_sexp a in
      let b = List.map b ~f:t_of_sexp in
      fold_right_non_empty (a, b) ~f:(fun a b -> Arr (a, b))
    | sexp -> of_sexp_error "Kind.t_of_sexp" sexp

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"F.Kind.t"
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
  module Map_type_rep = Type.Name.Make1 (struct
    let name = "F.Label.Map"
    type 'a t = 'a Map.t
  end)
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
  | Arr    of t * t (* CR: rename to Fun *)
  | Record of t Label.Map.t
  | Forall of (t Name.t * Kind.t Embed.t, t) Bind.t
  | Exists of (t Name.t * Kind.t Embed.t, t) Bind.t
  | Fun    of (t Name.t * Kind.t Embed.t, t) Bind.t (* CR: rename to Lambda *)
  | App    of t * t

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"F.Type.t"
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

  let rec subst t sub =
    match t with
    | Name b ->
      let (a, tsub) = sub in
      if Name.equal a b then tsub else t
    | Arr (t1, t2) -> Arr (subst t1 sub, subst t2 sub)
    | App (t1, t2) -> App (subst t1 sub, subst t2 sub)
    | Record m ->
      Record (Map.map m ~f:(fun t -> subst t sub))
    | Forall bnd -> Forall (subst_bnd bnd sub)
    | Exists bnd -> Exists (subst_bnd bnd sub)
    | Fun    bnd -> Fun    (subst_bnd bnd sub)

  and subst_bnd bnd sub =
    let (x, k, t) = unbind bnd in
    bind (x, k, subst t sub)

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
      | None -> Or_error.error "unbound type variable" a Name.sexp_of_t
      | Some k -> Ok k)
    | Arr (t1, t2) ->
      check_star ctx t1
      >>= fun () ->
      check_star ctx t2
      >>= fun () ->
      Ok Kind.Star
    | Record entries ->
      Or_error.all_ignore (List.map (Map.to_alist entries) ~f:(fun (_, t) -> check_star ctx t))
      >>= fun () ->
      Ok Kind.Star
    | Forall b | Exists b ->
      let (a, k, t) = unbind b in
      check_star (Context.add ctx a k) t
      >>= fun () ->
      Ok Kind.Star
    | Fun b ->
      let (a, k1, t) = unbind b in
      ok (Context.add ctx a k1) t
      >>= fun k2 ->
      Ok (Kind.Arr (k1, k2))
    | App (tfun, targ) ->
      ok ctx targ
      >>= fun karg ->
      ok ctx tfun
      >>= function
      | Kind.Arr (kdom, krng) ->
        if Kind.equal karg kdom then Ok krng else
          Or_error.error "argument kind mismatch" (`expected kdom, `observed karg)
          <:sexp_of<[`expected of Kind.t] * [`observed of Kind.t]>>
      | k -> Or_error.error "expected arrow kind instead of" k Kind.sexp_of_t

  and check_star ctx t =
    ok ctx t
    >>= function
    | Kind.Star -> Ok ()
    | k ->
      Or_error.error "kind mismatch" (`expected Kind.Star, `observed k)
      <:sexp_of<[`expected of Kind.t] * [`observed of Kind.t]>>

  module Sexp_conv : sig
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
  end = struct

    let rec unravel_bind t ~p =
      match p t with
      | None -> ([], t)
      | Some bnd ->
        let (a, k, t) = unbind bnd in
        let (aks, body) = unravel_bind t ~p in
        ((a, k) :: aks, body)

    let rec sexp_of_t = function
      | Name x -> Name.sexp_of_t x
      | Arr (a, b) ->
        let rec unravel = function
          | Arr (u, v) -> u :: unravel v
          | other -> [other]
        in
        Sexp.List (Sexp.Atom "Fun" :: List.map ~f:sexp_of_t (a :: unravel b))
      | Record m ->
        Sexp.List
          (Sexp.Atom "Record" :: List.map (Map.to_alist m) ~f:<:sexp_of<Label.t * t>>)
      | Forall bnd ->
        sexp_of_bnds (Forall bnd)
          ~name:"Forall"
          ~p:(function Forall bnd -> Some bnd | _ -> None)
      | Exists bnd ->
        sexp_of_bnds (Exists bnd)
          ~name:"Exists"
          ~p:(function Exists bnd -> Some bnd | _ -> None)
      | Fun bnd ->
        sexp_of_bnds (Fun bnd)
          ~name:"Lambda"
          ~p:(function Fun bnd -> Some bnd | _ -> None)
      | App (a, b) ->
        let rec app f args =
          match f with
          | App (p, n) -> app p (n :: args)
          | head -> (head, args)
        in
        let (head, args) = app a [b] in
        Sexp.List (sexp_of_t head :: List.map args ~f:sexp_of_t)

    and sexp_of_bnds t ~p ~name =
      let (aks, body) = unravel_bind t ~p in
      Sexp.List [
        Sexp.Atom name;
        <:sexp_of<(Name.t * Kind.t) list>> aks;
        sexp_of_t body;
      ]

    let rec t_of_sexp = function
      | Sexp.List (Sexp.Atom "Fun" :: a :: b) ->
        let a = t_of_sexp a in
        let b = List.map b ~f:t_of_sexp in
        fold_right_non_empty (a, b) ~f:(fun a b -> Arr (a, b))
      | Sexp.List (Sexp.Atom "Record" :: entries) as sexp ->
        begin
          match Label.Map.of_alist (List.map entries ~f:<:of_sexp<Label.t * t>>) with
          | `Ok map -> Record map
          | `Duplicate_key key ->
            let msg = sprintf "F.Ty.Record duplicate key: %s" (Label.to_string key) in
            of_sexp_error msg sexp
        end
      | Sexp.List [Sexp.Atom "Forall"; bnds; body] ->
        bnds_of_sexp bnds body ~c:(fun bnd -> Forall bnd)
      | Sexp.List [Sexp.Atom "Exists"; bnds; body] ->
        bnds_of_sexp bnds body ~c:(fun bnd -> Exists bnd)
      | Sexp.List [Sexp.Atom "Lambda"; bnds; body] ->
        bnds_of_sexp bnds body ~c:(fun bnd -> Fun bnd)
      | Sexp.Atom _ as sexp ->
        Name (Name.t_of_sexp sexp)
      | Sexp.List (fn :: args) ->
        List.fold ~init:(t_of_sexp fn) args ~f:(fun fn arg -> App (fn, t_of_sexp arg))
      | sexp -> of_sexp_error "F.Ty.t_of_sexp" sexp

    and bnds_of_sexp bnds body ~c =
      let aks = (<:of_sexp<(Name.t * Kind.t) list>> bnds) in
      List.fold_right aks ~init:(t_of_sexp body) ~f:(fun (a, k) t -> c (bind (a, k, t)))
  end

  include Sexp_conv

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

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"F.Term.t"
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
          | Tyfun  -> Bind.type_rep (Type.Rep.Pair (Ty.Name.type_rep, Embed.type_rep Kind.type_rep)) orep
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
    let x : t Name.t = x in
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

  let mk_tyfun a k body =
    let a : Ty.t Name.t = a in
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

  module X = Name

  module Name = struct
    include Name.Make (struct
      type nonrec a = t
      let name = "Type.Name"
      let type_rep = type_rep
    end)
    let to_label t = to_univ t |! Name.Univ.to_string |! Label.of_string
    let of_label l = Label.to_string l |! raw
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

  let rec ok ctx = function
    | Name x ->
      (match Context.find_tm ctx x with
      | None -> Or_error.error "unbound term variable" x Name.sexp_of_t
      | Some t -> Ok t)
    | Fun b ->
      let (x, t1, e) = un_fun b in
      Ty.check_star (Context.ty_ctx ctx) t1
      >>= fun () ->
      ok (Context.add_tm ctx x t1) e
      >>= fun t2 ->
      Ok (Ty.Arr (t1, t2))
    | App (efun, earg) ->
      begin
        ok ctx earg
        >>= fun targ ->
        ok ctx efun
        >>= function
        | Ty.Arr (tdom, trng) ->
          if Ty.equal targ tdom then Ok trng else
            Or_error.error "argument type mismatch" (`expected tdom, `observed targ)
            <:sexp_of<[`expected of Ty.t] * [`observed of Ty.t]>>
        | t -> Or_error.error "expected function type instead of" t Ty.sexp_of_t
      end
    | Record xes ->
      begin
        Map.to_alist xes
        |! List.map ~f:(fun (key, e) -> ok ctx e >>= fun t -> Ok (key, t))
        |! Or_error.all
        |! Or_error.map ~f:Label.Map.of_alist_exn
      end
      >>= fun types_by_field ->
      Ok (Ty.Record types_by_field)
    | Dot (e, x) ->
      begin
        ok ctx e
        >>= function
        | Ty.Record map ->
          begin
            match Label.Map.find map x with
            | Some t -> Ok t
            | None -> Or_error.error "undefined field" x Label.sexp_of_t
          end
        | t -> Or_error.error "expected record type rather than" t Ty.sexp_of_t
      end
    | Tyfun b ->
      let (a, k1, e) = un_tyfun b in
      ok (Context.add_ty ctx a k1) e
      >>= fun k2 ->
      Ok (Ty.forall (a, k1, k2))
    | Tyapp (e, targ) ->
      begin
        Ty.ok (Context.ty_ctx ctx) targ
        >>= fun karg ->
        ok ctx e
        >>= function
        | Ty.Forall bnd ->
          let (a, kdom, trng) = Ty.unbind bnd in
          if Kind.equal kdom karg then Ok (Ty.subst trng (a, targ)) else
            Or_error.error "argument kind mismatch" (`expected kdom, `observed karg)
            <:sexp_of<[`expected of Kind.t] * [`observed of Kind.t]>>
        | t -> Or_error.error "expected forall type rather than" t Ty.sexp_of_t
      end
    | Pack (tsub, e, exty) ->
      let (a, tintf) = un_pack exty in
      let ty_ctx = Context.ty_ctx ctx in
      Ty.ok ty_ctx tsub
      >>= fun k ->
      let t = Ty.exists (a, k, tintf) in
      Ty.check_star ty_ctx t
      >>= fun () ->
      ok ctx e
      >>= fun t_body ->
      let t_pkg = Ty.subst tintf (a, tsub) in
      if Ty.equal t_body t_pkg then Ok t else
        Or_error.error "package body type mismatch"
          (`expected t_pkg, `observed t_body)
        <:sexp_of<[`expected of Ty.t] * [`observed of Ty.t]>>
    | Unpack b ->
      begin
        let (a1, x, edef, ebody) = un_unpack b in
        ok ctx edef
        >>= function
        | Ty.Exists bnd ->
          let (a2, k, tbody) = Ty.unbind bnd in
          let (a, tbody) =
            let tbody = Ty.swap (a1, a2) tbody in
            (a1, tbody)
          in
          let ctx = Context.add_ty ctx a k in
          let ctx = Context.add_tm ctx x tbody in
          ok ctx ebody
          >>= fun t ->
          if Set.mem (Ty.fvs t) a
          then Or_error.error_string "existential type escaping its scope"
          else Ok t
        | t -> Or_error.error "expected an existential type instead of" t Ty.sexp_of_t
      end
    | Let b ->
      let (x, edef, ebody) = un_let b in
      ok ctx edef
      >>= fun t ->
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

  module Sexp_conv : sig
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
  end = struct

    let rec sexp_of_t = function
      | Name x -> Name.sexp_of_t x
      | Fun _ | Tyfun _ as fn -> sexp_of_fun fn
      | App _ | Tyapp _ as ap -> sexp_of_app ap
      | Record m ->
        Sexp.List
          (Sexp.Atom "Record" :: List.map (Map.to_alist m) ~f:<:sexp_of<Label.t * t>>)
      | Dot (m, x) ->
        let rec unravel m acc =
          match m with
          | Dot (m, x) -> unravel m (x :: acc)
          | head -> (head, acc)
        in
        let (m, xs) = unravel m [x] in
        Sexp.List (Sexp.Atom "Dot" :: sexp_of_t m :: List.map xs ~f:Label.sexp_of_t)
      | Pack (ty, tm_body, bnd) ->
        let (a, ty_body) = un_pack bnd in
        Sexp.List [
          Sexp.Atom "Pack";
          Ty.sexp_of_t ty;
          sexp_of_t tm_body;
          Sexp.Atom ":";
          Sexp.Atom "Exists";
          Ty.Name.sexp_of_t a;
          Sexp.Atom ".";
          Ty.sexp_of_t ty_body;
        ]
      | Unpack bnd ->
        let (a, x, scrutinee, body) = un_unpack bnd in
        Sexp.List [
          Sexp.Atom "Unpack";
          Ty.Name.sexp_of_t a;
          Name.sexp_of_t x;
          Sexp.Atom "=";
          sexp_of_t scrutinee;
          Sexp.Atom "in";
          sexp_of_t body;
        ]
      | Let _ as tm ->
        let rec unravel acc = function
          | Let bnd ->
            let (x, t, body) = un_let bnd in
            unravel ((x, t) :: acc) body
          | body -> (List.rev acc, body)
        in
        let (bnds, body) = unravel [] tm in
        Sexp.List [
          Sexp.Atom "Let";
          Sexp.List (List.map bnds ~f:<:sexp_of<Name.t * t>>);
          sexp_of_t body;
        ]

    and un_app f args =
      match f with
      | App (p, a) -> un_app p (`Tm a :: args)
      | Tyapp (p, a) -> un_app p (`Ty a :: args)
      | head -> (head, args)

    and sexp_of_app tm =
      let (head, args) = un_app tm [] in
      Sexp.List
        (sexp_of_t head
         :: List.map args ~f:(function
         | `Tm n -> sexp_of_t n
         | `Ty a -> Sexp.List [Sexp.Atom "Type"; Ty.sexp_of_t a]))

    and unravel_fun acc = function
      | Fun bnd ->
        let (x, a, body) = un_fun bnd in
        unravel_fun (`Tm (x, a) :: acc) body
      | Tyfun bnd ->
        let (a, k, body) = un_tyfun bnd in
        unravel_fun (`Ty (a, k) :: acc) body
      | other -> (List.rev acc, other)

    and sexp_of_fun tm =
      let (bnds, body) = unravel_fun [] tm in
      Sexp.List [
        Sexp.Atom "Fun";
        Sexp.List (List.map bnds ~f:(fun bnd ->
          Sexp.List begin
            match bnd with
            | `Tm (x, a) -> [Name.sexp_of_t x; Ty.sexp_of_t a]
            | `Ty (a, k) -> [Sexp.Atom "Type"; Ty.Name.sexp_of_t a; Kind.sexp_of_t k]
          end));
        sexp_of_t body;
      ]

    let rec t_of_sexp = function
      | Sexp.List [Sexp.Atom "Fun"; Sexp.List bnds; body] ->
        let bnd_of_sexp = function
          | Sexp.List [x; a] ->
            `Tm (Name.t_of_sexp x, Ty.t_of_sexp a)
          | Sexp.List [Sexp.Atom "Type"; a; k] ->
            `Ty (Ty.Name.t_of_sexp a, Kind.t_of_sexp k)
          | sexp -> of_sexp_error "F.Tm.Fun.bnd_of_sexp" sexp
        in
        List.map bnds ~f:bnd_of_sexp
        |! List.fold_right ~init:(t_of_sexp body) ~f:(fun bnd acc ->
          match bnd with
          | `Tm (x, a) -> mk_fun x a acc
          | `Ty (a, k) -> mk_tyfun a k acc)
      | Sexp.List [Sexp.Atom "Let"; Sexp.List bnds; body] ->
        List.map bnds ~f:<:of_sexp<Name.t * t>>
        |! List.fold_right ~init:(t_of_sexp body) ~f:(fun (x, tm) acc ->
          mk_let x tm acc)
      | Sexp.List [
          Sexp.Atom "Unpack"; a; x;
          Sexp.Atom "="; scrutinee;
          Sexp.Atom "in"; body;
      ] ->
        let a = Ty.Name.t_of_sexp a in
        let x = Name.t_of_sexp x in
        let scrutinee = t_of_sexp scrutinee in
        let body = t_of_sexp body in
        mk_unpack a x scrutinee body
      | Sexp.List [
          Sexp.Atom "Pack"; ty; tm_body;
          Sexp.Atom ":";
          Sexp.Atom "Exists"; a; Sexp.Atom "."; ty_body;
        ] ->
        let ty = Ty.t_of_sexp ty in
        let tm_body = t_of_sexp tm_body in
        let a = Ty.Name.t_of_sexp a in
        let ty_body = Ty.t_of_sexp ty_body in
        mk_pack ty tm_body (a, ty_body)
      | Sexp.List (Sexp.Atom "Record" :: entries) as sexp ->
        begin
          match Label.Map.of_alist (List.map entries ~f:<:of_sexp<Label.t * t>>) with
          | `Ok map -> Record map
          | `Duplicate_key label ->
            let msg = sprintf "F.Tm.Record.Duplicate_key %s" (Label.to_string label) in
            of_sexp_error msg sexp
        end
      | Sexp.List (Sexp.Atom "Dot" :: m :: xs) ->
        let xs = List.map xs ~f:Label.t_of_sexp in
        List.fold xs ~init:(t_of_sexp m) ~f:(fun acc x -> Dot (acc, x))
      | Sexp.Atom _ as x -> Name (Name.t_of_sexp x)
      | Sexp.List (fn :: args) ->
        let fn = t_of_sexp fn in
        let args =
          List.map args ~f:(function
          | Sexp.List [Sexp.Atom "Type"; a] -> `Ty (Ty.t_of_sexp a)
          | n -> `Tm (t_of_sexp n))
        in
        List.fold args ~init:fn ~f:(fun acc -> function
        | `Tm n -> App (acc, n)
        | `Ty a -> Tyapp (acc, a))
      | sexp -> of_sexp_error "F.Tm.t_of_sexp" sexp

  end
  include Sexp_conv

end

let rec subtype _ctx ~src ~dst =
  match (src, dst) with
  | (Ty.Name a, Ty.Name b) when Ty.Name.equal a b -> `Coerce (fun x -> x)
  | _ -> failwith "UNIMPLEMENTED: F.subtype"

