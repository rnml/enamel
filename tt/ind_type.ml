open Std_internal

type arg =
  | Rec of (Term.t, Term.t list) Term.Binds.t
  | Nonrec of Term.t
with sexp_of

type body = {
  tycon : Constant.t;
  kind : (Term.t, Level.t) Term.Binds.t;
  cons : (arg, Term.t list) Term.Binds.t Constant.Map.t;
} with sexp_of

type t = (Term.t, body) Term.Binds.t with sexp_of

let type_rep_of_arg : arg Type.Rep.t =
  Type.Rep.Variant (module struct
    type t = arg
    let name : arg Type.Name.t = Type.Name.create ~name:"Tt.Inductive_type.arg"
    module Label = struct
      type 'a t =
        | Rec : (Term.t, Term.t list) Term.Binds.t t
        | Nonrec : Term.t t
      let name_of : type a. a t -> string = function
        | Rec -> "rec"
        | Nonrec -> "nonrec"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Rec -> Term.Binds.type_rep Term.type_rep (Type.Rep.List Term.type_rep)
        | Nonrec -> Term.type_rep
      type univ = Label : 'a t -> univ
      let all = [Label Rec; Label Nonrec]
    end
    type 'a tag = 'a Label.t
    type rep = Tagged : 'a tag * 'a -> rep
    let project = function
      | Rec i    -> Tagged (Label.Rec, i)
      | Nonrec x -> Tagged (Label.Nonrec, x)
    let put (type a) (tag : a tag) (arg : a) : arg =
      match (tag, arg) with
      | (Label.Rec, i)    -> Rec i
      | (Label.Nonrec, x) -> Nonrec x
    let inject = fun (Tagged (tag, arg)) -> put tag arg
  end : Type.Rep.Variant.T with type t = arg)

let type_rep_of_body : body Type.Rep.t =
  Type.Rep.Record (module struct
    type t = body
    let name : t Type.Name.t =
      Type.Name.create ~name:"Tt.Inductive_type.body"
    module Label = struct
      type 'a t =
        | Tycon  : Constant.t t
        | Kind : (Term.t Term.s, Level.t) Bind.t t
        | Cons : (arg, Term.t list) Term.Binds.t Constant.Map.t t
      let name_of : type a. a t -> string = function
        | Tycon -> "tycon"
        | Kind  -> "kind"
        | Cons  -> "cons"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Tycon  -> Constant.type_rep
        | Kind   -> Term.Binds.type_rep Term.type_rep Level.type_rep
        | Cons ->
          Constant.type_rep_of_map
            (Term.Binds.type_rep type_rep_of_arg (Type.Rep.List Term.type_rep))
      type univ = Label : 'a t -> univ
      let all = [Label Tycon; Label Kind; Label Cons]
    end
    type 'a field = 'a Label.t
    type rep = { lookup : 'a. 'a field -> 'a }
    let inject {lookup} = {
      tycon = lookup Label.Tycon;
      kind  = lookup Label.Kind;
      cons  = lookup Label.Cons;
    }
    let get (type a) (f : a field) (t:t) : a =
      match f with
      | Label.Tycon -> t.tycon
      | Label.Kind  -> t.kind
      | Label.Cons  -> t.cons
    let project r = { lookup = fun field -> get field r }
  end : Type.Rep.Record.T with type t = body)

let type_rep = Term.Binds.type_rep Term.type_rep type_rep_of_body

let fun_map b ~args:(type_rep_of_a, f) ~body:(type_rep_of_b, g) =
  let (xas, body) = Term.Binds.unbind type_rep_of_a type_rep_of_b b in
  if List.is_empty xas then g body else Term.Fun (Term.Binds.bind (f xas, g body))

let kind t =
  let (params, body) = Term.Binds.unbind Term.type_rep type_rep_of_body t in
  fun_map body.kind
    ~args:(Term.type_rep, fun indices -> params @ indices)
    ~body:(Level.type_rep, fun level -> Term.Typ level)

let vars ctx = List.map ctx ~f:(fun (x, _) -> Term.Var x)

let force params : (Term.Name.t * _) list =
  List.map params ~f:(fun (x, a) ->
    let x =
      match x with
      | None -> Term.Name.create "x"
      | Some x -> x
    in
    (x, a))

let fill params = List.map params ~f:(fun (x, a) -> (Some x, a))

let cons t =
  let (params, body) = Term.Binds.unbind Term.type_rep type_rep_of_body t in
  let params = force params in
  let param_vars = vars params in
  let params = fill params in
  let ty_app indices = Term.App (Term.Con body.tycon, param_vars @ indices) in
  Map.map body.cons ~f:(fun b ->
    fun_map b
      ~body:(Type.Rep.List Term.type_rep, ty_app)
      ~args:(type_rep_of_arg, fun args ->
        params @
          List.map args ~f:(fun (arg_name, arg) ->
            (arg_name, begin
               match arg with
               | Nonrec t -> t
               | Rec b ->
                 fun_map b
                   ~args:(Term.type_rep, Fn.id)
                   ~body:(Type.Rep.List Term.type_rep, ty_app)
             end))))

let dummy name = Term.Name.freshen (Term.Name.create name)

let fun_bind = function
  | ([], body) -> body
  | bnd -> Term.Fun (Term.Binds.bind bnd)

let elim t =
  let (params, body) = Term.Binds.unbind Term.type_rep type_rep_of_body t in
  let params : (Term.Name.t * Term.t) list =
    List.map params ~f:(fun (x, a) ->
      let x =
        match x with
        | None -> Term.Name.create "x"
        | Some x -> x
      in
      (x, a))
  in
  let param_vars = vars params in
  let params : (Term.Name.t option * Term.t) list =
    List.map params ~f:(fun (x, a) -> (Some x, a))
  in
  let ty_app indices = Term.App (Term.Con body.tycon, param_vars @ indices) in
  let (indices, level) = Term.Binds.unbind Term.type_rep Level.type_rep body.kind in
  let indices = force indices in
  let index_vars = vars indices in
  let p = Term.Name.create "p" in
  let p_app args = Term.App (Term.Var p, args) in
  let motive =
    (Some p, fun_bind (fill indices @ [None, ty_app index_vars], Term.Typ level))
  in
  let means =
    List.map (Map.to_alist body.cons) ~f:(fun (con, con_ty) ->
      let (args, indices) =
        Term.Binds.unbind type_rep_of_arg (Type.Rep.List Term.type_rep) con_ty
      in
      let args = force args in
      let args' =
        List.concat_map args ~f:(fun (x, arg) ->
          match arg with
          | Nonrec t -> [(Some x, t)]
          | Rec b ->
            let (arg_args, indices) =
              Term.Binds.unbind Term.type_rep (Type.Rep.List Term.type_rep) b
            in
            let body = ty_app indices in
            let ihyp = p_app (indices @ [Term.Var x]) in
            [
              (Some x, fun_bind (arg_args, body));
              (None,   fun_bind (arg_args, ihyp));
            ])
      in
      ( Some (Term.Name.create (Constant.to_string con))
      , fun_bind
          ( args'
          , p_app (indices @ [Term.App (Term.Con con, param_vars @ vars args)]))))
  in
  let target = indices @ [dummy "tgt", ty_app index_vars] in
  let goal = p_app (vars target) in
  fun_bind (params @ [motive] @ means @ fill target, goal)

let pretty t =
  let (_params, body) =
    Bind.unbind (Term.type_rep_of_s Term.type_rep) type_rep_of_body t
  in
  let c = (Pretty.text "tycon = " ^^ Constant.pretty body.tycon) |> Pretty.agrp in
  let k  = (Pretty.text "kind = " ^^ Term.pretty (kind t)) |> Pretty.agrp in
  let cs =
    Map.map ~f:Term.pretty (cons t)
    |> Map.to_alist
    |> List.map ~f:(fun (c, p) ->
      Pretty.hgrp
        (Pretty.text "constructor = " ^^ Constant.pretty c ^+^ Pretty.text ":" ^+^ p))
    |> List.fold ~init:Pretty.empty ~f:(^+^)
    |> Pretty.vgrp
  in
  let e  =
    ( Pretty.text "elim ="
      ^^ Pretty.agrp (Pretty.nest 2 (Pretty.empty ^+^ Term.pretty (elim t)))
    ) |> Pretty.agrp
  in
  Pretty.vgrp (c ^+^ k ^+^ cs ^+^ e)

  (* let (params, body) = Bind.unbind (Term.type_rep_of_s Term.type_rep) type_rep_of_body t in
   * Pretty.text "data "
   * ^^ Constant.pretty body.tycon
   * ^+^
   *   (if (match params with Term.Nil -> true | _ -> false)
   *    then Pretty.empty
   *    else Pretty.text "(" ^^ Term.pretty_s Term.pretty params ^^ Pretty.text ")")
   * ^+^ Pretty.text ":" *)

