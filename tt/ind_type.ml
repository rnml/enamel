open Std_internal

type arg =
  | Rec of Term.t list
  | Nonrec of Term.t

type body = {
  tycon : Constant.t;
  kind : (Term.t Term.s, Level.t) Bind.t;
  tmcons :
    ((Term.t, arg) Bind.t Term.s, Term.t list) Bind.t Constant.Map.t;
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
        | Tmcons : ((Term.t, arg) Bind.t Term.s, Term.t list) Bind.t Constant.Map.t t
      let name_of : type a. a t -> string = function
        | Tycon  -> "tycon"
        | Kind   -> "kind"
        | Tmcons -> "tmcons"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Tycon  -> Constant.type_rep
        | Kind   -> Bind.type_rep (Term.type_rep_of_s Term.type_rep) Level.type_rep
        | Tmcons ->
          Constant.type_rep_of_map
            (Bind.type_rep
               (Term.type_rep_of_s
                  (Bind.type_rep Term.type_rep type_rep_of_arg))
               (Type.Rep.List Term.type_rep))
      type univ = Label : 'a t -> univ
      let all = [Label Tycon; Label Kind; Label Tmcons]
    end
    type 'a field = 'a Label.t
    type rep = { lookup : 'a. 'a field -> 'a }
    let inject {lookup} = {
      tycon  = lookup Label.Tycon;
      kind   = lookup Label.Kind;
      tmcons = lookup Label.Tmcons;
    }
    let get (type a) (f : a field) (t:t) : a =
      match f with
      | Label.Tycon  -> t.tycon
      | Label.Kind   -> t.kind
      | Label.Tmcons -> t.tmcons
    let project r = { lookup = fun field -> get field r }
  end : Type.Rep.Record.T with type t = body)

let type_rep =
  Bind.type_rep (Term.type_rep_of_s Term.type_rep) type_rep_of_body

  (*
   data T (Gamma) : (Delta) -> Type =
   | ...
   | C (Gamma) : (Theta) -> T (Gamma) (Row)
   | ...
*)

