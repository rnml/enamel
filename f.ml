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
    | Name   of Name.t
    | Fun    of (t * t)
    | Record of t Label.Map.t
    | Forall of (Name.t * Kind.t Embed.t, t) Bind.t
    | Exists of (Name.t * Kind.t Embed.t, t) Bind.t
    | Lam    of (Name.t * Kind.t Embed.t, t) Bind.t
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

  (* TODO: add subst to Term_tc and Pattern_tc, just like in the paper *)
  (* let subst t (x, n) =
   *   let t = tc.open Name.ptc Nat.zero x t in
   *   tc.close Name.ptc Nat.zero  : 'p. 'p Pattern_tc.t -> Nat.t -> 'p -> 'a -> 'a;
   *
   *   close    : 'p. 'p t -> Nat.t -> 'p -> 'a -> 'a;
   *   open_    : 'p. 'p t -> Nat.t -> 'p -> 'a -> 'a; *)

  let equal t1 t2 = compare t1 t2 = 0

  module Context : sig
    type t
    val empty   : t
    val add   : t -> Name.t -> Kind.t -> t
    val find  : t -> Name.t -> Kind.t option
  end = struct
    type t = Kind.t Name.Map.t
    let empty = Name.Map.empty
    let add g a k = Map.add g ~key:a ~data:k
    let find g x = Map.find g x
  end
end

(* module Term = struct
 *
 *   type t =
 *     | Name of t Name.t
 *     | Fun of (t Name.t * Ty.t Embed.t, t) Bind.t
 *     | App of t * t
 *     | Record of t Label.Map.t
 *     | Dot of t * Label.t
 *     | Tyfun of (Ty.Name.t * Kind.t Embed.t, t) Bind.t
 *     | Tyapp of t * Ty.t
 *     | Pack of Ty.t * t * (Ty.Name.t, Ty.t) Bind.t
 *     | Unpack of (Ty.Name.t * t Name.t * t Embed.t, t) Bind.t
 *     | Let of (t Name.t * t Embed.t, t) Bind.t
 *
 *   module X = Name
 *
 *   module Name = struct
 *     include Name.Make (struct
 *       type nonrec a = t
 *       let name = "Type.Name"
 *     end)
 *     let to_label t = to_univ t |> Name.Univ.to_string |> Label.of_string
 *     let of_label l = Label.to_string l |> raw
 *   end
 *
 *   module Context : sig
 *     type t
 *     val empty   : t
 *     val add_tm  : t -> Name.t -> Ty.t -> t
 *     val find_tm : t -> Name.t -> Ty.t option
 *     val add_ty  : t -> Ty.Name.t -> Kind.t -> t
 *     val ty_ctx  : t -> Ty.Context.t
 *   end = struct
 *
 *     type t = {
 *       ty_ctx : Ty.Context.t;
 *       tm_ctx : Ty.t Name.Map.t;
 *     }
 *
 *     let empty = {
 *       ty_ctx = Ty.Context.empty;
 *       tm_ctx = Name.Map.empty;
 *     }
 *
 *     let add_ty g a k = {g with ty_ctx = Ty.Context.add g.ty_ctx a k}
 *
 *     let ty_ctx g = g.ty_ctx
 *
 *     let add_tm g x s =
 *       {g with tm_ctx = Map.add g.tm_ctx ~key:x ~data:s}
 *
 *     let find_tm g x = Map.find g.tm_ctx x
 *   end
 *
 * end
*)
