open Std_internal

type arg =
  | Rec of Term.t list
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t, Level.t) Term.Binds.t;
  cons : ((Term.t, arg) Term.Binds.t, Term.t list) Term.Binds.t Constant.Map.t;
}

type t = (Term.t, body) Term.Binds.t

let type_rep_of_arg : arg Type.Rep.t =
  Type.Rep.Variant (module struct
    type t = arg
    let name : arg Type.Name.t = Type.Name.create ~name:"Tt.Inductive_type.arg"
    module Label = struct
      type 'a t =
        | Rec : Term.t list t
        | Nonrec : Term.t t
      let name_of : type a. a t -> string = function
        | Rec -> "rec"
        | Nonrec -> "nonrec"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Rec -> Type.Rep.List Term.type_rep
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
        | Cons :
            ( (Term.t, arg) Term.Binds.t
            , Term.t list
            ) Term.Binds.t Constant.Map.t t
      let name_of : type a. a t -> string = function
        | Tycon -> "tycon"
        | Kind  -> "kind"
        | Cons  -> "cons"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Tycon  -> Constant.type_rep
        | Kind   -> Term.Binds.type_rep Term.type_rep Level.type_rep
        | Cons ->
          Constant.type_rep_of_map
            (Term.Binds.type_rep
               (Term.Binds.type_rep Term.type_rep type_rep_of_arg)
               (Type.Rep.List Term.type_rep))
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

let type_rep =
  Bind.type_rep (Term.type_rep_of_s Term.type_rep) type_rep_of_body

let kind t =
  let (params, body) =
    Bind.unbind (Term.type_rep_of_s Term.type_rep)
      type_rep_of_body t
  in
  let params = Term.unbind_s params in
  let (indices, univ) =
    Bind.unbind (Term.type_rep_of_s Term.type_rep) Level.type_rep
      body.kind
  in
  let indices = Term.unbind_s indices in
  Term.Fun
    (Bind.create (Term.bind_s (params @ indices)) (Term.Typ univ))

let cons t =
  let (params, body) =
    Term.Binds.unbind Term.type_rep type_rep_of_body t
  in
  let param_args =
    List.map params ~f:(fun (x, _) -> Term.Var x)
  in
  let ty_app indices =
    Term.App (Term.Con body.tycon, param_args @ indices)
  in
  Map.map body.cons ~f:(fun b ->
    let (args, indices) =
      Term.Binds.unbind
        (Term.Binds.type_rep Term.type_rep type_rep_of_arg)
        (Type.Rep.List Term.type_rep)
        b
    in
    let args =
      List.map args ~f:(fun (arg_name, arg) ->
        let arg =
          let (arg_args, arg_ty) =
            Term.Binds.unbind Term.type_rep type_rep_of_arg arg
          in
          let arg_ty =
            match arg_ty with
            | Rec indices -> ty_app indices
            | Nonrec t -> t
          in
          Term.Fun (Term.Binds.bind (arg_args, arg_ty))
        in
        (arg_name, arg))
    in
    Term.Fun (Term.Binds.bind (params @ args, ty_app indices)))

let elim _ = assert false

