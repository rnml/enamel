open Std_internal

type t =
  | Typ of Level.t
  | Var of t Name.t
  | Con of Constant.t
  | Lam of (t s, t) Bind.t
  | App of t * t list
  | Fun of (t s, t) Bind.t

and 'a s = (* telescope *)
  | Nil
  | Cons of (t Name.t * 'a Embed.t, 'a s) Rebind.t

let rec type_rep : t Type.Rep.t =
  Type.Rep.Variant (module struct
    type o = t
    let orep = type_rep
    type t = o
    let name : t Type.Name.t = Type.Name.create ~name:"Tt.Term.t"
    module Label = struct
      type 'a t =
        | Typ : Level.t t
        | Var : o Name.t t
        | Con : Constant.t t
        | Lam : (o s, o) Bind.t t
        | App : (o * o list) t
        | Fun : (o s, o) Bind.t t
      let name_of : type a. a t -> string = function
        | Typ -> "typ"
        | Var -> "var"
        | Con -> "con"
        | Lam -> "lam"
        | App -> "app"
        | Fun -> "fun"
      let type_of : type a. a t -> a Type.Rep.t = function
        | Typ -> Type.Rep.Int
        | Var -> Name.type_rep orep
        | Con -> Constant.type_rep
        | Lam -> Bind.type_rep (type_rep_of_s orep) orep
        | App -> Type.Rep.Pair (orep, Type.Rep.List orep)
        | Fun -> Bind.type_rep (type_rep_of_s orep) orep
      type univ = Label : 'a t -> univ
      let all = [Label Typ; Label Var; Label Con; Label Lam; Label App; Label Fun]
    end
    type 'a tag = 'a Label.t
    type rep = Tagged : 'a tag * 'a -> rep
    let project = function
      | Typ i      -> Tagged (Label.Typ, i)
      | Var x      -> Tagged (Label.Var, x)
      | Con c      -> Tagged (Label.Con, c)
      | Lam b      -> Tagged (Label.Lam, b)
      | App (m, n) -> Tagged (Label.App, (m, n))
      | Fun b      -> Tagged (Label.Fun, b)
    let put (type a) (tag : a tag) (arg : a) : t =
      match (tag, arg) with
      | (Label.Typ, i)      -> Typ i
      | (Label.Var, x)      -> Var x
      | (Label.Con, c)      -> Con c
      | (Label.Lam, b)      -> Lam b
      | (Label.App, (m, n)) -> App (m, n)
      | (Label.Fun, b)      -> Fun b
    let inject = fun (Tagged (tag, arg)) -> put tag arg
  end : Type.Rep.Variant.T with type t = t)

and type_rep_of_s : 'a. 'a Type.Rep.t -> 'a s Type.Rep.t =
  fun (type a) arep ->
    let rec sa_rep : a s Type.Rep.t =
      Type.Rep.Variant (module struct
        type o = t
        type t = a s
        let name : t Type.Name.t = Type.Name.create ~name:"Tt.Term.t"
        module Label = struct
          type 'a t =
            | Nil : unit t
            | Cons : (o Name.t * a Embed.t, a s) Rebind.t t
          let name_of : type a. a t -> string = function
            | Nil -> "nil"
            | Cons -> "cons"
          let type_of : type a. a t -> a Type.Rep.t = function
            | Nil -> Type.Rep.Unit
            | Cons ->
              Rebind.type_rep
                (Type.Rep.Pair (Name.type_rep type_rep, Embed.type_rep arep))
                sa_rep
          type univ = Label : 'a t -> univ
          let all = [Label Nil; Label Cons]
        end
        type 'a tag = 'a Label.t
        type rep = Tagged : 'a tag * 'a -> rep
        let project = function
          | Nil    -> Tagged (Label.Nil, ())
          | Cons b -> Tagged (Label.Cons, b)
        let put (type a) (tag : a tag) (arg : a) : t =
          match (tag, arg) with
          | (Label.Nil, ()) -> Nil
          | (Label.Cons, b) -> Cons b
        let inject = fun (Tagged (tag, arg)) -> put tag arg
      end : Type.Rep.Variant.T with type t = a s)
    in
    sa_rep

module X = Name

module Name = Name.Make (struct
  type nonrec a = t
  let name = "Tt.Term.Name"
  let type_rep = type_rep
end)

let rec unbind_s : 'a. 'a s -> (Name.t * 'a) list = fun (type a) -> function
  | Nil -> []
  | Cons r ->
    let ((x, a), s) = ((r : (Name.t * a Embed.t, a s) Rebind.t) :> ((Name.t * a) * a s)) in
    (x, a) :: unbind_s s

let bind (xas, t) =
  let s =
    List.fold_right xas ~init:Nil ~f:(fun (x, a) s ->
      Cons (Rebind.create (x, Embed.create a) s))
  in
  Bind.create s t

let unbind b =
  let (s, t) = Bind.unbind (type_rep_of_s type_rep) type_rep b in
  (unbind_s s, t)

