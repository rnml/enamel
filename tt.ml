open Std_internal

module Term = struct

  type t =
    | Typ of int
    | Var of t Name.t
    | Con of Constant.t
    | Lam of (s, t) Bind.t
    | App of t * t list
    | Fun of (s, t) Bind.t

  and s = (* telescope *)
    | Nil
    | Cons of (t Name.t * t Embed.t, s) Rebind.t

  let rec type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let orep = type_rep
      type t = o
      let name : t Type.Name.t = Type.Name.create ~name:"Tt.Term.t"
      module Label = struct
        type 'a t =
          | Typ : int t
          | Var : o Name.t t
          | Con : Constant.t t
          | Lam : (s, o) Bind.t t
          | App : (o * o list) t
          | Fun : (s, o) Bind.t t
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
          | Lam -> Bind.type_rep s_type_rep orep
          | App -> Type.Rep.Pair (orep, Type.Rep.List orep)
          | Fun -> Bind.type_rep s_type_rep orep
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

  and s_type_rep =
    Type.Rep.Variant (module struct
      type o = t
      let srep = s_type_rep
      type t = s
      let name : t Type.Name.t = Type.Name.create ~name:"Tt.Term.t"
      module Label = struct
        type 'a t =
          | Nil : unit t
          | Cons : (o Name.t * o Embed.t, s) Rebind.t t
        let name_of : type a. a t -> string = function
          | Nil -> "nil"
          | Cons -> "cons"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Nil -> Type.Rep.Unit
          | Cons ->
            Rebind.type_rep
              (Type.Rep.Pair (Name.type_rep type_rep, Embed.type_rep type_rep))
              srep
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
    end : Type.Rep.Variant.T with type t = s)

  module X = Name

  module Name = Name.Make (struct
    type nonrec a = t
    let name = "Tt.Term.Name"
    let type_rep = type_rep
  end)

  let bind (xas, t) =
    let s =
      List.fold_right xas ~init:Nil ~f:(fun (x, a) s ->
        Cons (Rebind.create (x, Embed.create a) s))
    in
    Bind.create s t

  let unbind b =
    let (s, t) = Bind.unbind s_type_rep type_rep b in
    let rec aux = function
      | Nil -> []
      | Cons r ->
        let ((x, a), s) = (r :> ((Name.t * t Embed.t) * s)) in
        let a = (a :> t) in
        (x, a) :: aux s
    in
    (aux s, t)

end

