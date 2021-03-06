open Std_internal

open Or_error.Monad_infix

module Label : Identifiable = String_id

module F = struct

  module Kind = struct
    type t =
      | Type
      | Fun of t * t
    with compare, sexp

    let tc = Term_tc.const ~cmp:compare

    let compare = tc.compare
    let equal   = Term_tc.equal tc
  end

  module Type = struct

    module Name =
      Make_name_type (struct
        let module_name = "F.Type.Name"
      end)

    type t =
      | Name   of (Name.t)
      | Fun    of (t * t)
      | Record of (t Label.Map.t)
      | Forall of ((Name.t * Kind.t Embed.t, t) Bind.t)
      | Exists of ((Name.t * Kind.t Embed.t, t) Bind.t)
      | Lambda of ((Name.t * Kind.t Embed.t, t) Bind.t)
      | App    of (t * t)
    with sexp

    let rec tc : t Term_tc.t = {
      close = (fun ptc l p t ->
        match t with
        | Name   x -> let tc = Lazy.force name_tc in Name   (tc.close ptc l p x)
        | Fun    x -> let tc = Lazy.force pair_tc in Fun    (tc.close ptc l p x)
        | Record x -> let tc = Lazy.force lmap_tc in Record (tc.close ptc l p x)
        | Forall x -> let tc = Lazy.force bind_tc in Forall (tc.close ptc l p x)
        | Exists x -> let tc = Lazy.force bind_tc in Exists (tc.close ptc l p x)
        | Lambda x -> let tc = Lazy.force bind_tc in Lambda (tc.close ptc l p x)
        | App    x -> let tc = Lazy.force pair_tc in App    (tc.close ptc l p x)
      );
      open_ = (fun ptc l p t ->
        match t with
        | Name   x -> let tc = Lazy.force name_tc in Name   (tc.open_ ptc l p x)
        | Fun    x -> let tc = Lazy.force pair_tc in Fun    (tc.open_ ptc l p x)
        | Record x -> let tc = Lazy.force lmap_tc in Record (tc.open_ ptc l p x)
        | Forall x -> let tc = Lazy.force bind_tc in Forall (tc.open_ ptc l p x)
        | Exists x -> let tc = Lazy.force bind_tc in Exists (tc.open_ ptc l p x)
        | Lambda x -> let tc = Lazy.force bind_tc in Lambda (tc.open_ ptc l p x)
        | App    x -> let tc = Lazy.force pair_tc in App    (tc.open_ ptc l p x)
      );
      compare = (fun x y ->
        match (x, y) with
        | (Name   x, Name   y) -> let tc = Lazy.force name_tc in tc.compare x y
        | (Fun    x, Fun    y) -> let tc = Lazy.force pair_tc in tc.compare x y
        | (Record x, Record y) -> let tc = Lazy.force lmap_tc in tc.compare x y
        | (Forall x, Forall y) -> let tc = Lazy.force bind_tc in tc.compare x y
        | (Exists x, Exists y) -> let tc = Lazy.force bind_tc in tc.compare x y
        | (Lambda x, Lambda y) -> let tc = Lazy.force bind_tc in tc.compare x y
        | (App    x, App    y) -> let tc = Lazy.force pair_tc in tc.compare x y
        | (Name   _ , _) -> -1 | (_ , Name   _) ->  1
        | (Fun    _ , _) -> -1 | (_ , Fun    _) ->  1
        | (Record _ , _) -> -1 | (_ , Record _) ->  1
        | (Forall _ , _) -> -1 | (_ , Forall _) ->  1
        | (Exists _ , _) -> -1 | (_ , Exists _) ->  1
        | (Lambda _ , _) -> -1 | (_ , Lambda _) ->  1
        (* | (App    _ , _) -> -1 | (_ , App    _) ->  1 *)
      );
      fv = (function
        | Name   x -> let tc = Lazy.force name_tc in tc.fv x
        | Fun    x -> let tc = Lazy.force pair_tc in tc.fv x
        | Record x -> let tc = Lazy.force lmap_tc in tc.fv x
        | Forall x -> let tc = Lazy.force bind_tc in tc.fv x
        | Exists x -> let tc = Lazy.force bind_tc in tc.fv x
        | Lambda x -> let tc = Lazy.force bind_tc in tc.fv x
        | App    x -> let tc = Lazy.force pair_tc in tc.fv x
      );
    }

    and name_tc : Name.t Term_tc.t Lazy.t =
      lazy Name.tc

    and bind_tc : (Name.t * Kind.t Embed.t, t) Bind.t Term_tc.t Lazy.t =
      lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc)

    and pair_tc : (t * t) Term_tc.t Lazy.t =
      lazy (Term_tc.pair tc tc)

    and lmap_tc : t Label.Map.t Term_tc.t Lazy.t =
      lazy (Term_tc.map tc)

    let compare = tc.compare
    let equal   = Term_tc.equal tc

    let fv t =
      tc.fv t
      |> Set.to_list
      |> List.filter_map ~f:Name.match_
      |> Name.Set.of_list

    module Shape = struct
      type 'a t =
        | Name   of Name.t
        | Fun    of 'a * 'a
        | Record of 'a Label.Map.t
        | Forall of Name.t * Kind.t * 'a
        | Exists of Name.t * Kind.t * 'a
        | Lambda of Name.t * Kind.t * 'a
        | App    of 'a * 'a

      let rec map t ~f =
        match t with
        | Name x -> Name x
        | Fun (a, b) -> Fun (f a, f b)
        | Record a -> Record (Map.map ~f a)
        | Forall (x, a, b) -> Forall (x, a, f b)
        | Exists (x, a, b) -> Exists (x, a, f b)
        | Lambda (x, a, b) -> Lambda (x, a, f b)
        | App (a, b) -> App (f a, f b)
    end

    let create : t Shape.t -> t = function
      | Name x -> Name x
      | App (a, b) -> App (a, b)
      | Fun (a, b) -> Fun (a, b)
      | Record a -> Record a
      | Forall (x, arg_type, body) ->
        let arg_type = Embed.create Kind.tc arg_type in
        let bind =
          Bind.create (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc (x, arg_type) body
        in
        Forall bind
      | Exists (x, arg_type, body) ->
        let arg_type = Embed.create Kind.tc arg_type in
        let bind =
          Bind.create (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc (x, arg_type) body
        in
        Exists bind
      | Lambda (x, arg_type, body) ->
        let arg_type = Embed.create Kind.tc arg_type in
        let bind =
          Bind.create (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc (x, arg_type) body
        in
        Lambda bind

    let match_ : t -> t Shape.t = function
      | Name x -> Name x
      | App (a, b) -> App (a, b)
      | Fun (a, b) -> Fun (a, b)
      | Record a -> Record a
      | Forall bind ->
        let ((x, arg_type), body) =
          Bind.expose (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc bind
        in
        let arg_type = Embed.expose Kind.tc arg_type in
        Forall (x, arg_type, body)
      | Exists bind ->
        let ((x, arg_type), body) =
          Bind.expose (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc bind
        in
        let arg_type = Embed.expose Kind.tc arg_type in
        Exists (x, arg_type, body)
      | Lambda bind ->
        let ((x, arg_type), body) =
          Bind.expose (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) tc bind
        in
        let arg_type = Embed.expose Kind.tc arg_type in
        Lambda (x, arg_type, body)

    let fv t =
      tc.fv t
      |> Set.to_list
      |> List.filter_map ~f:Name.match_
      |> Name.Set.of_list

    let equal t1 t2 = compare t1 t2 = 0

    let rec subst t sub =
      match match_ t with
      | Name x -> if Name.equal x (fst sub) then snd sub else t
      | Fun    (a, b)    -> create (Fun (subst a sub, subst b sub))
      | Record map       -> create (Record (Map.map map ~f:(fun t -> subst t sub)))
      | Forall (x, k, a) -> create (Forall (x, k, subst a sub))
      | Exists (x, k, a) -> create (Exists (x, k, subst a sub))
      | Lambda (x, k, a) -> create (Lambda (x, k, subst a sub))
      | App    (a, b)    -> create (App (subst a sub, subst b sub))

  end

  module Term = struct

    module Name = struct
      include Make_name_type (struct
        let module_name = "F.Term.Name"
      end)
      let to_label t = to_string t |> Label.of_string
      let of_label l = Label.to_string l |> of_string
    end

    type t =
      | Name   of (Name.t)
      | Lambda of (Name.t * Type.t Embed.t, t) Bind.t
      | App    of (t * t)
      | Record of (t Label.Map.t)
      | Dot    of (t * Label.t)
      | Tyfun  of ((Type.Name.t * Kind.t Embed.t, t) Bind.t)
      | Tyapp  of (t * Type.t)
      | Pack   of (Type.t * t * (Type.Name.t, Type.t) Bind.t)
      | Unpack of ((Type.Name.t * Name.t * t Embed.t, t) Bind.t)
      | Let    of ((Name.t * t Embed.t, t) Bind.t)
    with sexp

    let rec tc : t Term_tc.t = {
      close = (fun ptc l p t ->
        match t with
        | Name   x -> let tc = Lazy.force name_tc   in Name   (tc.close ptc l p x)
        | Lambda x -> let tc = Lazy.force lambda_tc in Lambda (tc.close ptc l p x)
        | App    x -> let tc = Lazy.force app_tc    in App    (tc.close ptc l p x)
        | Record x -> let tc = Lazy.force record_tc in Record (tc.close ptc l p x)
        | Dot    x -> let tc = Lazy.force dot_tc    in Dot    (tc.close ptc l p x)
        | Tyfun  x -> let tc = Lazy.force tyfun_tc  in Tyfun  (tc.close ptc l p x)
        | Tyapp  x -> let tc = Lazy.force tyapp_tc  in Tyapp  (tc.close ptc l p x)
        | Pack   x -> let tc = Lazy.force pack_tc   in Pack   (tc.close ptc l p x)
        | Unpack x -> let tc = Lazy.force unpack_tc in Unpack (tc.close ptc l p x)
        | Let    x -> let tc = Lazy.force let_tc    in Let    (tc.close ptc l p x)
      );
      open_ = (fun ptc l p t ->
        match t with
        | Name   x -> let tc = Lazy.force name_tc   in Name   (tc.open_ ptc l p x)
        | Lambda x -> let tc = Lazy.force lambda_tc in Lambda (tc.open_ ptc l p x)
        | App    x -> let tc = Lazy.force app_tc    in App    (tc.open_ ptc l p x)
        | Record x -> let tc = Lazy.force record_tc in Record (tc.open_ ptc l p x)
        | Dot    x -> let tc = Lazy.force dot_tc    in Dot    (tc.open_ ptc l p x)
        | Tyfun  x -> let tc = Lazy.force tyfun_tc  in Tyfun  (tc.open_ ptc l p x)
        | Tyapp  x -> let tc = Lazy.force tyapp_tc  in Tyapp  (tc.open_ ptc l p x)
        | Pack   x -> let tc = Lazy.force pack_tc   in Pack   (tc.open_ ptc l p x)
        | Unpack x -> let tc = Lazy.force unpack_tc in Unpack (tc.open_ ptc l p x)
        | Let    x -> let tc = Lazy.force let_tc    in Let    (tc.open_ ptc l p x)
      );
      compare = (fun x y ->
        match (x, y) with
        | (Name   x, Name   y) -> let tc = Lazy.force name_tc   in tc.compare x y
        | (Lambda x, Lambda y) -> let tc = Lazy.force lambda_tc in tc.compare x y
        | (App    x, App    y) -> let tc = Lazy.force app_tc    in tc.compare x y
        | (Record x, Record y) -> let tc = Lazy.force record_tc in tc.compare x y
        | (Dot    x, Dot    y) -> let tc = Lazy.force dot_tc    in tc.compare x y
        | (Tyfun  x, Tyfun  y) -> let tc = Lazy.force tyfun_tc  in tc.compare x y
        | (Tyapp  x, Tyapp  y) -> let tc = Lazy.force tyapp_tc  in tc.compare x y
        | (Pack   x, Pack   y) -> let tc = Lazy.force pack_tc   in tc.compare x y
        | (Unpack x, Unpack y) -> let tc = Lazy.force unpack_tc in tc.compare x y
        | (Let    x, Let    y) -> let tc = Lazy.force let_tc    in tc.compare x y
        | (Name   _, _) -> -1 | (_, Name   _) -> 1
        | (Lambda _, _) -> -1 | (_, Lambda _) -> 1
        | (App    _, _) -> -1 | (_, App    _) -> 1
        | (Record _, _) -> -1 | (_, Record _) -> 1
        | (Dot    _, _) -> -1 | (_, Dot    _) -> 1
        | (Tyfun  _, _) -> -1 | (_, Tyfun  _) -> 1
        | (Tyapp  _, _) -> -1 | (_, Tyapp  _) -> 1
        | (Pack   _, _) -> -1 | (_, Pack   _) -> 1
        | (Unpack _, _) -> -1 | (_, Unpack _) -> 1
        (* | (Let    _, _) -> -1 | (_, Let    _) -> 1 *)
      );
      fv = (function
        | Name   x -> let tc = Lazy.force name_tc   in tc.fv x
        | Lambda x -> let tc = Lazy.force lambda_tc in tc.fv x
        | App    x -> let tc = Lazy.force app_tc    in tc.fv x
        | Record x -> let tc = Lazy.force record_tc in tc.fv x
        | Dot    x -> let tc = Lazy.force dot_tc    in tc.fv x
        | Tyfun  x -> let tc = Lazy.force tyfun_tc  in tc.fv x
        | Tyapp  x -> let tc = Lazy.force tyapp_tc  in tc.fv x
        | Pack   x -> let tc = Lazy.force pack_tc   in tc.fv x
        | Unpack x -> let tc = Lazy.force unpack_tc in tc.fv x
        | Let    x -> let tc = Lazy.force let_tc    in tc.fv x
      );
    }

    and name_tc   : (Name.t)                                       Term_tc.t Lazy.t = lazy (Name.tc)
    and lambda_tc : (Name.t * Type.t Embed.t, t) Bind.t            Term_tc.t Lazy.t = lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) tc)
    and app_tc    : (t * t)                                        Term_tc.t Lazy.t = lazy (Term_tc.pair tc tc)
    and record_tc : (t Label.Map.t)                                Term_tc.t Lazy.t = lazy (Term_tc.map tc)
    and dot_tc    : (t * Label.t)                                  Term_tc.t Lazy.t = lazy (Term_tc.pair tc (Term_tc.const ~cmp:<:compare<Label.t>>))
    and tyfun_tc  : ((Type.Name.t * Kind.t Embed.t, t) Bind.t)     Term_tc.t Lazy.t = lazy (Bind.tc (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) tc)
    and tyapp_tc  : (t * Type.t)                                   Term_tc.t Lazy.t = lazy (Term_tc.pair tc Type.tc)
    and pack_tc   : (Type.t * t * (Type.Name.t, Type.t) Bind.t)    Term_tc.t Lazy.t = lazy (Term_tc.triple Type.tc tc (Bind.tc Type.Name.ptc Type.tc))
    and unpack_tc : ((Type.Name.t * Name.t * t Embed.t, t) Bind.t) Term_tc.t Lazy.t = lazy (Bind.tc (Pattern_tc.triple Type.Name.ptc Name.ptc (Embed.tc tc)) tc)
    and let_tc    : ((Name.t * t Embed.t, t) Bind.t)               Term_tc.t Lazy.t = lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) tc)

    let compare = tc.compare
    let equal   = Term_tc.equal tc

    module Shape = struct
      type 'a t =
        | Name   of Name.t
        | Lambda of Name.t * Type.t * 'a
        | App    of 'a * 'a
        | Record of 'a Label.Map.t
        | Dot    of 'a * Label.t
        | Tyfun  of Type.Name.t * Kind.t * 'a
        | Tyapp  of 'a * Type.t
        | Pack   of Type.t * 'a * Type.Name.t * Type.t (* pack <ty, tm> : exists a. ty *)
        | Unpack of Type.Name.t * Name.t * 'a * 'a     (* let pack <a, x> = e in e     *)
        | Let    of Name.t * 'a * 'a                   (* let x = e in e               *)
    end

    let create : t Shape.t -> t = function
      | Name x -> Name x
      | App (m, n) -> App (m, n)
      | Lambda (x, a, m) ->
        let a = Embed.create Type.tc a in
        let bind =
          Bind.create (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) tc (x, a) m
        in
        Lambda bind
      | Tyapp (m, n) -> Tyapp (m, n)
      | Tyfun (x, a, m) ->
        let a = Embed.create Kind.tc a in
        let bind =
          Bind.create (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) tc (x, a) m
        in
        Tyfun bind
      | Record r -> Record r
      | Dot (m, x) -> Dot (m, x)
      | Pack (a, m, b, c) ->
        let bind = Bind.create Type.Name.ptc Type.tc b c in
        Pack (a, m, bind)
      | Unpack (a, x, m, n) ->
        let m = Embed.create tc m in
        let bind =
          Bind.create (Pattern_tc.triple Type.Name.ptc Name.ptc (Embed.tc tc)) tc (a, x, m) n
        in
        Unpack bind
      | Let (x, m, n) ->
        let m = Embed.create tc m in
        let bind =
          Bind.create (Pattern_tc.pair Name.ptc (Embed.tc tc)) tc (x, m) n
        in
        Let bind

    let match_ : t -> t Shape.t = function
      | Name x -> Name x
      | App (m, n) -> App (m, n)
      | Lambda bind ->
        let ((x, a), m) =
          Bind.expose (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) tc bind
        in
        let a = Embed.expose Type.tc a in
        Lambda (x, a, m)
      | Tyapp (m, n) -> Tyapp (m, n)
      | Tyfun bind ->
        let ((x, a), m) =
          Bind.expose (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) tc bind
        in
        let a = Embed.expose Kind.tc a in
        Tyfun (x, a, m)
      | Record r -> Record r
      | Dot (m, x) -> Dot (m, x)
      | Pack (a, m, bind) ->
        let (b, c) = Bind.expose Type.Name.ptc Type.tc bind in
        Pack (a, m, b, c)
      | Unpack bind ->
        let ((a, x, m), n) =
          Bind.expose (Pattern_tc.triple Type.Name.ptc Name.ptc (Embed.tc tc)) tc bind
        in
        let m = Embed.expose tc m in
        Unpack (a, x, m, n)
      | Let bind ->
        let ((x, m), n) =
          Bind.expose (Pattern_tc.pair Name.ptc (Embed.tc tc)) tc bind
        in
        let m = Embed.expose tc m in
        Let (x, m, n)

    let term_fv t =
      tc.fv t
      |> Set.to_list
      |> List.filter_map ~f:Name.match_
      |> Name.Set.of_list

    let type_fv t =
      tc.fv t
      |> Set.to_list
      |> List.filter_map ~f:Type.Name.match_
      |> Type.Name.Set.of_list

    let rec type_subst t sub =
      let tm t = type_subst t sub in
      let ty t = Type.subst t sub in
      match match_ t with
      | Name   (a)          -> create @@ Name a
      | Lambda (a, b, c)    -> create @@ Lambda (a, ty b, tm c)
      | App    (a, b)       -> create @@ App (tm a, tm b)
      | Record (a)          -> create @@ Record (Map.map ~f:tm a)
      | Dot    (a, b)       -> create @@ Dot (tm a, b)
      | Tyfun  (a, b, c)    -> create @@ Tyfun (a, b, tm c)
      | Tyapp  (a, b)       -> create @@ Tyapp (tm a, ty b)
      | Pack   (a, b, c, d) -> create @@ Pack (ty a, tm b, c, ty d)
      | Unpack (a, b, c, d) -> create @@ Unpack (a, b, tm c, tm d)
      | Let    (a, b, c)    -> create @@ Let (a, tm b, tm c)

    let rec subst t sub =
      let tm t = subst t sub in
      let ty t = t in
      match match_ t with
      | Name x -> if Name.equal x (fst sub) then snd sub else t
      | Lambda (a, b, c)    -> create @@ Lambda (a, ty b, tm c)
      | App    (a, b)       -> create @@ App (tm a, tm b)
      | Record (a)          -> create @@ Record (Map.map ~f:tm a)
      | Dot    (a, b)       -> create @@ Dot (tm a, b)
      | Tyfun  (a, b, c)    -> create @@ Tyfun (a, b, tm c)
      | Tyapp  (a, b)       -> create @@ Tyapp (tm a, ty b)
      | Pack   (a, b, c, d) -> create @@ Pack (ty a, tm b, c, ty d)
      | Unpack (a, b, c, d) -> create @@ Unpack (a, b, tm c, tm d)
      | Let    (a, b, c)    -> create @@ Let (a, tm b, tm c)

  end

end

module Target = struct

  module rec Csig : sig

    type t =
      | Val    of (F.Type.t)
      | Type   of (F.Type.t * F.Kind.t)
      | Sig    of (Asig.t)
      | Struct of (t Label.Map.t)
      | Forall of (((F.Type.Name.t * F.Kind.t Embed.t) list, t * Asig.t) Bind.t)

    val tc : t Term_tc.t Lazy.t

    module Shape : sig
      type nonrec 'a t =
        | Val    of F.Type.t
        | Type   of F.Type.t * F.Kind.t
        | Sig    of Asig.t
        | Struct of 'a Label.Map.t
        | Forall of (F.Type.Name.t * F.Kind.t) list * 'a * Asig.t
      val map : 'a1 t -> f:('a1 -> 'a2) -> 'a2 t
    end

    val create : t Shape.t -> t
    val match_ : t -> t Shape.t

    val equal : t -> t -> bool

  end = struct

    type t =
      | Val    of (F.Type.t)
      | Type   of (F.Type.t * F.Kind.t)
      | Sig    of (Asig.t)
      | Struct of (t Label.Map.t)
      | Forall of (((F.Type.Name.t * F.Kind.t Embed.t) list, t * Asig.t) Bind.t)

    let rec tc : t Term_tc.t Lazy.t = lazy {
      close = (fun ptc l p t ->
        match t with
        | Val    x -> let tc = Lazy.force val_tc    in Val    (tc.close ptc l p x)
        | Type   x -> let tc = Lazy.force type_tc   in Type   (tc.close ptc l p x)
        | Sig    x -> let tc = Lazy.force sig_tc    in Sig    (tc.close ptc l p x)
        | Struct x -> let tc = Lazy.force struct_tc in Struct (tc.close ptc l p x)
        | Forall x -> let tc = Lazy.force forall_tc in Forall (tc.close ptc l p x)
      );
      open_ = (fun ptc l p t ->
        match t with
        | Val    x -> let tc = Lazy.force val_tc    in Val    (tc.open_ ptc l p x)
        | Type   x -> let tc = Lazy.force type_tc   in Type   (tc.open_ ptc l p x)
        | Sig    x -> let tc = Lazy.force sig_tc    in Sig    (tc.open_ ptc l p x)
        | Struct x -> let tc = Lazy.force struct_tc in Struct (tc.open_ ptc l p x)
        | Forall x -> let tc = Lazy.force forall_tc in Forall (tc.open_ ptc l p x)
      );
      compare = (fun x y ->
        match (x, y) with
        | (Val    x, Val    y) -> let tc = Lazy.force val_tc    in tc.compare x y
        | (Type   x, Type   y) -> let tc = Lazy.force type_tc   in tc.compare x y
        | (Sig    x, Sig    y) -> let tc = Lazy.force sig_tc    in tc.compare x y
        | (Struct x, Struct y) -> let tc = Lazy.force struct_tc in tc.compare x y
        | (Forall x, Forall y) -> let tc = Lazy.force forall_tc in tc.compare x y
        | (Val    _, _) -> -1 | (_, Val    _) -> 1
        | (Type   _, _) -> -1 | (_, Type   _) -> 1
        | (Sig    _, _) -> -1 | (_, Sig    _) -> 1
        | (Struct _, _) -> -1 | (_, Struct _) -> 1
        (* | (Forall _, _) -> -1 | (_, Forall _) -> 1 *)
      );
      fv = (function
        | Val    x -> let tc = Lazy.force val_tc    in tc.fv x
        | Type   x -> let tc = Lazy.force type_tc   in tc.fv x
        | Sig    x -> let tc = Lazy.force sig_tc    in tc.fv x
        | Struct x -> let tc = Lazy.force struct_tc in tc.fv x
        | Forall x -> let tc = Lazy.force forall_tc in tc.fv x
      );
    }

    and val_tc : F.Type.t Term_tc.t Lazy.t =
      lazy F.Type.tc

    and type_tc : (F.Type.t * F.Kind.t) Term_tc.t Lazy.t =
      lazy (Term_tc.pair F.Type.tc F.Kind.tc)

    and sig_tc : Asig.t Term_tc.t Lazy.t =
      lazy (Lazy.force Asig.tc)

    and struct_tc : t Label.Map.t Term_tc.t Lazy.t =
      lazy (Term_tc.map (Lazy.force tc))

    and forall_tc : ( (F.Type.Name.t * F.Kind.t Embed.t) list
                 , t * Asig.t
                 ) Bind.t Term_tc.t Lazy.t =
      lazy (Bind.tc
              (Pattern_tc.list (Pattern_tc.pair
                                  F.Type.Name.ptc
                                  (Embed.tc F.Kind.tc)))
              (Term_tc.pair (Lazy.force tc) (Lazy.force Asig.tc)))

    let equal t1 t2 = (Lazy.force tc).compare t1 t2 = 0

    module Shape = struct
      type nonrec 'a t =
        | Val    of F.Type.t
        | Type   of F.Type.t * F.Kind.t
        | Sig    of Asig.t
        | Struct of 'a Label.Map.t
        | Forall of (F.Type.Name.t * F.Kind.t) list * 'a * Asig.t

      let map t ~f =
        match t with
        | Val    (a)       -> Val (a)
        | Type   (a, b)    -> Type (a, b)
        | Sig    (a)       -> Sig (a)
        | Struct (a)       -> Struct (Label.Map.map ~f a)
        | Forall (a, b, c) -> Forall (a, f b, c)
    end

    let create : t Shape.t -> t = function
      | Val (a)     -> Val (a)
      | Type (a, b) -> Type (a, b)
      | Sig (a)     -> Sig (a)
      | Struct (a)  -> Struct (a)
      | Forall (args, csig, asig) ->
        let args =
          List.map args ~f:(fun (a, b) ->
            let b = Embed.create F.Kind.tc b in
            (a, b))
        in
        let bind =
          Bind.create
            (Pattern_tc.list (Pattern_tc.pair F.Type.Name.ptc (Embed.tc F.Kind.tc)))
            (Term_tc.pair (Lazy.force tc) (Lazy.force Asig.tc))
            args (csig, asig)
        in
        Forall bind

    let match_ : t -> t Shape.t = function
      | Val (a)     -> Val (a)
      | Type (a, b) -> Type (a, b)
      | Sig (a)     -> Sig (a)
      | Struct (a)  -> Struct (a)
      | Forall bind ->
        let (args, (csig, asig)) =
          Bind.expose
            (Pattern_tc.list (Pattern_tc.pair F.Type.Name.ptc (Embed.tc F.Kind.tc)))
            (Term_tc.pair (Lazy.force tc) (Lazy.force Asig.tc))
            bind
        in
        let args =
          List.map args ~f:(fun (a, b) ->
            let b = Embed.expose F.Kind.tc b in
            (a, b))
        in
        Forall (args, csig, asig)

  end

  and Asig : sig
    type t =
      | Exists of (((F.Type.Name.t * F.Kind.t Embed.t) list, Csig.t) Bind.t)
    with compare

    val tc : t Term_tc.t Lazy.t

    module Shape : sig
      type t =
        | Exists of (F.Type.Name.t * F.Kind.t) list * Csig.t
    end

    val create : Shape.t -> t
    val match_ : t -> Shape.t

  end = struct
    type t =
      | Exists of (((F.Type.Name.t * F.Kind.t Embed.t) list, Csig.t) Bind.t)

    let rec tc : t Term_tc.t Lazy.t = lazy {
      close = (fun ptc l p t ->
        match t with
        | Exists x -> let tc = Lazy.force exists_tc in Exists (tc.close ptc l p x)
      );
      open_ = (fun ptc l p t ->
        match t with
        | Exists x -> let tc = Lazy.force exists_tc in Exists (tc.open_ ptc l p x)
      );
      compare = (fun x y ->
        match (x, y) with
        | (Exists x, Exists y) -> let tc = Lazy.force exists_tc in tc.compare x y
      );
      fv = (function
        | Exists x -> let tc = Lazy.force exists_tc in tc.fv x
      );
    }

    and exists_tc :
      ((F.Type.Name.t * F.Kind.t Embed.t) list, Csig.t) Bind.t Term_tc.t Lazy.t =
      lazy (Bind.tc
              (Pattern_tc.list (Pattern_tc.pair F.Type.Name.ptc (Embed.tc F.Kind.tc)))
              (Lazy.force Csig.tc))

    let compare a b = (Lazy.force tc).compare a b

    module Shape = struct
      type t =
        | Exists of (F.Type.Name.t * F.Kind.t) list * Csig.t
    end

    let create : Shape.t -> t = function
      | Exists (b, c) ->
        let b =
          List.map b ~f:(fun (c, d) ->
            let d = Embed.create F.Kind.tc d in
            (c, d))
        in
        let a =
          Bind.create
            (Pattern_tc.list (Pattern_tc.pair F.Type.Name.ptc (Embed.tc F.Kind.tc)))
            (Lazy.force Csig.tc) b c
        in
        Exists a

    let match_ : t -> Shape.t = function
      | Exists a ->
        let (b, c) =
          Bind.expose
            (Pattern_tc.list (Pattern_tc.pair F.Type.Name.ptc (Embed.tc F.Kind.tc)))
            (Lazy.force Csig.tc) a
        in
        let b =
          List.map b ~f:(fun (c, d) ->
            let d = Embed.expose F.Kind.tc d in
            (c, d))
        in
        Exists (b, c)

  end

end
