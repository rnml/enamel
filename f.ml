open Std_internal

open Or_error.Monad_infix

module Kind = struct
  type t =
    | Type
    | Fun of t * t
  with compare

  let tc = Term_tc.const ~cmp:compare

  let compare = tc.compare
  let equal   = Term_tc.equal tc
end

module Label : Identifiable = String_id

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
    | Lam    of ((Name.t * Kind.t Embed.t, t) Bind.t)
    | App    of (t * t)
  with compare

  let rec tc : t Term_tc.t = {
    close = (fun ptc l p t ->
      match t with
      | Name   x -> let tc = Lazy.force name_tc in Name   (tc.close ptc l p x)
      | Fun    x -> let tc = Lazy.force pair_tc in Fun    (tc.close ptc l p x)
      | Record x -> let tc = Lazy.force lmap_tc in Record (tc.close ptc l p x)
      | Forall x -> let tc = Lazy.force bind_tc in Forall (tc.close ptc l p x)
      | Exists x -> let tc = Lazy.force bind_tc in Exists (tc.close ptc l p x)
      | Lam    x -> let tc = Lazy.force bind_tc in Lam    (tc.close ptc l p x)
      | App    x -> let tc = Lazy.force pair_tc in App    (tc.close ptc l p x)
    );
    open_ = (fun ptc l p t ->
      match t with
      | Name   x -> let tc = Lazy.force name_tc in Name   (tc.open_ ptc l p x)
      | Fun    x -> let tc = Lazy.force pair_tc in Fun    (tc.open_ ptc l p x)
      | Record x -> let tc = Lazy.force lmap_tc in Record (tc.open_ ptc l p x)
      | Forall x -> let tc = Lazy.force bind_tc in Forall (tc.open_ ptc l p x)
      | Exists x -> let tc = Lazy.force bind_tc in Exists (tc.open_ ptc l p x)
      | Lam    x -> let tc = Lazy.force bind_tc in Lam    (tc.open_ ptc l p x)
      | App    x -> let tc = Lazy.force pair_tc in App    (tc.open_ ptc l p x)
    );
    compare;
    fv = (function
      | Name   x -> let tc = Lazy.force name_tc in tc.fv x
      | Fun    x -> let tc = Lazy.force pair_tc in tc.fv x
      | Record x -> let tc = Lazy.force lmap_tc in tc.fv x
      | Forall x -> let tc = Lazy.force bind_tc in tc.fv x
      | Exists x -> let tc = Lazy.force bind_tc in tc.fv x
      | Lam    x -> let tc = Lazy.force bind_tc in tc.fv x
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
     | Lam    of Name.t * Kind.t * 'a
     | App    of 'a * 'a

    let rec map t ~f =
      match t with
      | Name x -> Name x
      | Fun (a, b) -> Fun (f a, f b)
      | Record a -> Record (Map.map ~f a)
      | Forall (x, a, b) -> Forall (x, a, f b)
      | Exists (x, a, b) -> Exists (x, a, f b)
      | Lam (x, a, b) -> Lam (x, a, f b)
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
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) (x, arg_type) body
      in
      Forall bind
    | Exists (x, arg_type, body) ->
      let arg_type = Embed.create Kind.tc arg_type in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) (x, arg_type) body
      in
      Exists bind
    | Lam (x, arg_type, body) ->
      let arg_type = Embed.create Kind.tc arg_type in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) (x, arg_type) body
      in
      Lam bind

  let match_ : t -> t Shape.t = function
    | Name x -> Name x
    | App (a, b) -> App (a, b)
    | Fun (a, b) -> Fun (a, b)
    | Record a -> Record a
    | Forall bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) bind
      in
      let arg_type = Embed.expose Kind.tc arg_type in
      Forall (x, arg_type, body)
    | Exists bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) bind
      in
      let arg_type = Embed.expose Kind.tc arg_type in
      Exists (x, arg_type, body)
    | Lam bind ->
      let ((x, arg_type), body) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Kind.tc)) bind
      in
      let arg_type = Embed.expose Kind.tc arg_type in
      Lam (x, arg_type, body)

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
    | Lam    (x, k, a) -> create (Lam (x, k, subst a sub))
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
    | Fun    of (Name.t * Type.t Embed.t, t) Bind.t
    | App    of (t * t)
    | Record of (t Label.Map.t)
    | Dot    of (t * Label.t)
    | Tyfun  of ((Type.Name.t * Kind.t Embed.t, t) Bind.t)
    | Tyapp  of (t * Type.t)
    | Pack   of (Type.t * t * (Type.Name.t, Type.t) Bind.t)
    | Unpack of ((Type.Name.t * Name.t * t Embed.t, t) Bind.t)
    | Let    of ((Name.t * t Embed.t, t) Bind.t)
  with compare

  let rec tc : t Term_tc.t = {
    close = (fun ptc l p t ->
      match t with
      | Name   x -> let tc = Lazy.force name_tc   in Name   (tc.close ptc l p x)
      | Fun    x -> let tc = Lazy.force fun_tc    in Fun    (tc.close ptc l p x)
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
      | Fun    x -> let tc = Lazy.force fun_tc    in Fun    (tc.open_ ptc l p x)
      | App    x -> let tc = Lazy.force app_tc    in App    (tc.open_ ptc l p x)
      | Record x -> let tc = Lazy.force record_tc in Record (tc.open_ ptc l p x)
      | Dot    x -> let tc = Lazy.force dot_tc    in Dot    (tc.open_ ptc l p x)
      | Tyfun  x -> let tc = Lazy.force tyfun_tc  in Tyfun  (tc.open_ ptc l p x)
      | Tyapp  x -> let tc = Lazy.force tyapp_tc  in Tyapp  (tc.open_ ptc l p x)
      | Pack   x -> let tc = Lazy.force pack_tc   in Pack   (tc.open_ ptc l p x)
      | Unpack x -> let tc = Lazy.force unpack_tc in Unpack (tc.open_ ptc l p x)
      | Let    x -> let tc = Lazy.force let_tc    in Let    (tc.open_ ptc l p x)
    );
    compare;
    fv = (function
      | Name   x -> let tc = Lazy.force name_tc   in tc.fv x
      | Fun    x -> let tc = Lazy.force fun_tc    in tc.fv x
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
  and fun_tc    : (Name.t * Type.t Embed.t, t) Bind.t            Term_tc.t Lazy.t = lazy (Bind.tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) tc)
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
      | Fun    of Name.t * Type.t * 'a
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
    | Fun (x, a, m) ->
      let a = Embed.create Type.tc a in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) (x, a) m
      in
      Fun bind
    | Tyapp (m, n) -> Tyapp (m, n)
    | Tyfun (x, a, m) ->
      let a = Embed.create Kind.tc a in
      let bind =
        Bind.create tc (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) (x, a) m
      in
      Tyfun bind
    | Record r -> Record r
    | Dot (m, x) -> Dot (m, x)
    | Pack (a, m, b, c) ->
      let bind = Bind.create Type.tc Type.Name.ptc b c in
      Pack (a, m, bind)
    | Unpack (a, x, m, n) ->
      let m = Embed.create tc m in
      let bind =
        Bind.create tc (Pattern_tc.triple Type.Name.ptc Name.ptc (Embed.tc tc)) (a, x, m) n
      in
      Unpack bind
    | Let (x, m, n) ->
      let m = Embed.create tc m in
      let bind =
        Bind.create tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) (x, m) n
      in
      Let bind

  let match_ : t -> t Shape.t = function
    | Name x -> Name x
    | App (m, n) -> App (m, n)
    | Fun bind ->
      let ((x, a), m) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc Type.tc)) bind
      in
      let a = Embed.expose Type.tc a in
      Fun (x, a, m)
    | Tyapp (m, n) -> Tyapp (m, n)
    | Tyfun bind ->
      let ((x, a), m) =
        Bind.expose tc (Pattern_tc.pair Type.Name.ptc (Embed.tc Kind.tc)) bind
      in
      let a = Embed.expose Kind.tc a in
      Tyfun (x, a, m)
    | Record r -> Record r
    | Dot (m, x) -> Dot (m, x)
    | Pack (a, m, bind) ->
      let (b, c) = Bind.expose Type.tc Type.Name.ptc bind in
      Pack (a, m, b, c)
    | Unpack bind ->
      let ((a, x, m), n) =
        Bind.expose tc (Pattern_tc.triple Type.Name.ptc Name.ptc (Embed.tc tc)) bind
      in
      let m = Embed.expose tc m in
      Unpack (a, x, m, n)
    | Let bind ->
      let ((x, m), n) =
        Bind.expose tc (Pattern_tc.pair Name.ptc (Embed.tc tc)) bind
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
    match match_ t with
    | Fun    (a, b)    -> create (Fun (subst a sub, subst b sub))
    | Record map       -> create (Record (Map.map map ~f:(fun t -> subst t sub)))
    | Forall (x, k, a) -> create (Forall (x, k, subst a sub))
    | Exists (x, k, a) -> create (Exists (x, k, subst a sub))
    | Lam    (x, k, a) -> create (Lam (x, k, subst a sub))
    | App    (a, b)    -> create (App (subst a sub, subst b sub))

      | Name _ -> t
      | Fun    (x, a, m) -> create @@ Fun (x, Type.subst a sub, type_subst m sub)
      | App    (m, n)    -> create @@ App (type_subst m sub, type_subst n sub)
      | Record map       -> create @@ Record (Map.map map ~f:(fun t -> type_subst t sub))
      | Dot    of 'a * Label.t
      | Tyfun  of Type.Name.t * Kind.t * 'a
      | Tyapp  of 'a * Type.t
      | Pack   of Type.t * 'a * Type.Name.t * Type.t (* pack <ty, tm> : exists a. ty *)
      | Unpack of Type.Name.t * Name.t * 'a * 'a     (* let pack <a, x> = e in e     *)
      | Let    of Name.t * 'a * 'a                   (* let x = e in e               *)

end
