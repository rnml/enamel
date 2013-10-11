open Std_internal

module Binds = struct

  type ('a, 'b) t = ('a Term.s, 'b) Bind.t

  type ('a, 'b) e = (Term.Name.t * 'a) list * 'b

  let type_rep a b = Bind.type_rep (Term.type_rep_of_s a) b

  let unbind typerep_of_a typerep_of_b t =
    let (s, b) = Bind.unbind (Term.type_rep_of_s typerep_of_a) typerep_of_b t in
    let xas = Term.unbind_s s in
    (xas, b)

  let bind (xas, b) = Bind.create (Term.bind_s xas) b

end

type arg =
  | Rec of Term.t list
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t, Level.t) Binds.t;
  cons :
    ((Term.t, arg) Binds.t, Term.t list) Binds.t Constant.Map.t;
}

type t = (Term.t Term.s, body) Bind.t

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
            ( (Term.t Term.s, arg) Bind.t Term.s
            , Term.t list
            ) Bind.t Constant.Map.t t
      let name_of : type a. a t -> string = function
        | Tycon -> "tycon"
        | Kind  -> "kind"
        | Cons  -> "cons"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Tycon  -> Constant.type_rep
        | Kind   -> Bind.type_rep (Term.type_rep_of_s Term.type_rep) Level.type_rep
        | Cons ->
          Constant.type_rep_of_map
            (Bind.type_rep
               (Term.type_rep_of_s
                  (Bind.type_rep
                     (Term.type_rep_of_s Term.type_rep)
                     type_rep_of_arg))
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
    Bind.unbind (Term.type_rep_of_s Term.type_rep)
      type_rep_of_body t
  in
  let params = Term.unbind_s params in
  let ty_app indices =
    let ty_args =
      List.map params ~f:(fun (x, _) -> Term.Var x) @ indices
    in
    Term.App (Term.Con body.tycon, ty_args)
  in
  Map.map body.cons ~f:(fun b ->
    let (args, indices) =
      Bind.unbind
        (Term.type_rep_of_s
           (Bind.type_rep
              (Term.type_rep_of_s Term.type_rep)
              type_rep_of_arg))
        (Type.Rep.List Term.type_rep) b
    in
    let args = Term.unbind_s args in
    let args =
      params @ begin
        List.map args ~f:(fun (arg_name, arg) ->
          let arg =
            let (arg_args, arg_ty) =
              Bind.unbind (Term.type_rep_of_s Term.type_rep)
                type_rep_of_arg arg
            in
            let arg_ty =
              match arg_ty with
              | Rec indices -> ty_app indices
              | Nonrec t -> t
            in
            Term.Fun (Bind.create arg_args arg_ty)
          in
          (arg_name, arg))
      end
    in
    Term.Fun
      (Bind.create
         (Term.bind_s (params @ args))
         (ty_app indices)))

let elim _ = assert false

