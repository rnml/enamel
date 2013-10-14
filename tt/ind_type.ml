open Std_internal

type arg =
  | Rec of (Term.t, Term.t list) Term.Binds.t
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t, Level.t) Term.Binds.t;
  cons : (arg, Term.t list) Term.Binds.t Constant.Map.t;
}

type t = (Term.t, body) Term.Binds.t

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

let kind t =
  let (params, body) = Term.Binds.unbind Term.type_rep type_rep_of_body t in
  Term.Fun begin
    Term.Binds.map body.kind
      ~args:(Term.type_rep, fun indices -> params @ indices)
      ~body:(Level.type_rep, fun level -> Term.Typ level)
  end

let vars ctx = List.map ctx ~f:(fun (x, _) -> Term.Var x)

let cons t =
  let (params, body) = Term.Binds.unbind Term.type_rep type_rep_of_body t in
  let param_vars = vars params in
  let ty_app indices = Term.App (Term.Con body.tycon, param_vars @ indices) in
  Map.map body.cons ~f:(fun b ->
    Term.Fun begin
      Term.Binds.map b
        ~body:(Type.Rep.List Term.type_rep, ty_app)
        ~args:(type_rep_of_arg, fun args ->
          params @
            List.map args ~f:(fun (arg_name, arg) ->
              (arg_name, begin
                 match arg with
                 | Nonrec t -> t
                 | Rec b ->
                   Term.Fun begin
                     Term.Binds.map b
                       ~args:(Term.type_rep, Fn.id)
                       ~body:(Type.Rep.List Term.type_rep, ty_app)
                   end
               end)))
    end)

let dummy name = Term.Name.freshen (Term.Name.create name)

let fun_bind bnd = Term.Fun (Term.Binds.bind bnd)

let elim t =
  let (params, body) = Term.Binds.unbind Term.type_rep type_rep_of_body t in
  let param_vars = vars params in
  let ty_app indices = Term.App (Term.Con body.tycon, param_vars @ indices) in
  let (indices, level) = Term.Binds.unbind Term.type_rep Level.type_rep body.kind in
  let index_vars = vars indices in
  let p = Term.Name.create "p" in
  let p_app args = Term.App (Term.Var p, args) in
  let motive = (p, fun_bind (indices @ [dummy "", ty_app index_vars], Term.Typ level)) in
  let means =
    List.map (Map.to_alist body.cons) ~f:(fun (con, con_ty) ->
      let (args, indices) =
        Term.Binds.unbind type_rep_of_arg (Type.Rep.List Term.type_rep) con_ty
      in
      let args' =
        List.concat_map args ~f:(fun (x, arg) ->
          match arg with
          | Nonrec t -> [(x, t)]
          | Rec b ->
            let (arg_args, indices) =
              Term.Binds.unbind Term.type_rep (Type.Rep.List Term.type_rep) b
            in
            let body = ty_app indices in
            let px = Term.Name.freshen x in
            let ind_hyp = p_app (indices @ [Term.Var x]) in
            [
              (x,  fun_bind (arg_args, body));
              (px, fun_bind (arg_args, ind_hyp));
            ])
      in
      ( Term.Name.create (Constant.to_string con)
      , fun_bind
          ( args'
          , p_app (indices @ [Term.App (Term.Con con, param_vars @ vars args)]))))
  in
  let target = indices @ [dummy "tgt", ty_app index_vars] in
  let goal = p_app (vars target) in
  fun_bind (params @ [motive] @ means @ target, goal)

let pretty t =
  let (_params, body) =
    Bind.unbind (Term.type_rep_of_s Term.type_rep) type_rep_of_body t
  in
  let c  = Constant.pretty body.tycon in
  let k  = Term.pretty (kind t) in
  let cs =
    Map.map ~f:Term.pretty (cons t)
    |> Map.to_alist
    |> List.map ~f:(fun (c, p) ->
      Pretty.hgrp (Constant.pretty c ^+^ Pretty.text ":" ^+^ p))
    |> List.fold ~init:Pretty.empty ~f:(^^)
  in
  let e  = Term.pretty (elim t) in
  Pretty.vgrp (c ^+^ k ^+^ cs ^+^ e)

  (* let (params, body) = Bind.unbind (Term.type_rep_of_s Term.type_rep) type_rep_of_body t in
   * Pretty.text "data "
   * ^^ Constant.pretty body.tycon
   * ^+^
   *   (if (match params with Term.Nil -> true | _ -> false)
   *    then Pretty.empty
   *    else Pretty.text "(" ^^ Term.pretty_s Term.pretty params ^^ Pretty.text ")")
   * ^+^ Pretty.text ":" *)

