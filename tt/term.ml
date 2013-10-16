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
  | Cons of (t Name.t option * 'a Embed.t, 'a s) Rebind.t
with sexp_of

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
        | Typ -> Level.type_rep
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
            | Cons : (o Name.t option * a Embed.t, a s) Rebind.t t
          let name_of : type a. a t -> string = function
            | Nil -> "nil"
            | Cons -> "cons"
          let type_of : type a. a t -> a Type.Rep.t = function
            | Nil -> Type.Rep.Unit
            | Cons ->
              Rebind.type_rep
                (Type.Rep.Pair
                   ( Type.Rep.Option (Name.type_rep type_rep)
                   , Embed.type_rep arep ))
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

let rec unbind_s : 'a. 'a s -> (Name.t option * 'a) list = fun (type a) -> function
  | Nil -> []
  | Cons r ->
    let ((x, a), s) =
      ( (* safe cast *)
        (r : (Name.t option * a Embed.t, a s) Rebind.t)
        :> ((Name.t option * a) * a s) )
    in
    (x, a) :: unbind_s s

let bind_s xas =
  List.fold_right xas ~init:Nil ~f:(fun (x, a) s ->
    Cons (Rebind.create (x, Embed.create a) s))

module Binds = struct

  type ('a, 'b) t = ('a s, 'b) Bind.t with sexp_of

  type ('a, 'b) e = (Name.t option * 'a) list * 'b

  let type_rep a b = Bind.type_rep (type_rep_of_s a) b

  let unbind typerep_of_a typerep_of_b t =
    let (s, b) = Bind.unbind (type_rep_of_s typerep_of_a) typerep_of_b t in
    let xas = unbind_s s in
    (xas, b)

  let bind (xas, b) = Bind.create (bind_s xas) b

  let map t ~args:(type_rep_of_a, f) ~body:(type_rep_of_b, g) =
    let (xas, b) = unbind type_rep_of_a type_rep_of_b t in
    bind (f xas, g b)

end

let bind = Binds.bind

let unbind_raw b = Bind.unbind (type_rep_of_s type_rep) type_rep b

let unbind b = let (s, t) = unbind_raw b in (unbind_s s, t)

let concat2 (type a) s1 s2 =
  let rec loop = function
    | Nil -> s2
    | Cons r ->
      let ((x, a), s) =
        ( (* safe cast *)
          (r : (Name.t option * a Embed.t, a s) Rebind.t)
          :> ((Name.t option  * a) * a s) )
      in
      Cons (Rebind.create (x, Embed.create a) (loop s))
  in
  loop s1

let concat ss = List.fold_right ss ~init:Nil ~f:concat2

let paren x p =
  if x then Pretty.text "(" ^^ p ^^ Pretty.text ")" else p

let pcat ps = List.fold ~init:Pretty.empty ~f:(^^) ps

let pretty_s pretty_a s =
  let bnds =
    unbind_s s
    |> List.group ~break:(fun _ b -> Option.is_none (fst b))
    |> List.map ~f:(fun chunk ->
      let ps =
        List.map chunk ~f:(fun (x, a) ->
          match x with
          | None -> pretty_a a
          | Some x ->
            Pretty.fgrp (
              Pretty.text (Name.to_string x ^ " :") ^^ begin
                Pretty.nest 2 (Pretty.break ^^ pretty_a a)
              end))
      in
      let named = Option.is_some (fst (List.hd_exn chunk)) in
      paren named (pcat begin
        if named
        then List.intersperse ~sep:(Pretty.text "," ^^ Pretty.break) ps
        else ps
      end))
  in
  let bnds = List.intersperse ~sep:(Pretty.text "->" ^^ Pretty.break) bnds in
  if List.is_empty bnds
  then Pretty.empty
  else Pretty.agrp (pcat bnds)

let rec pretty p = function
  | Typ _ -> Pretty.text "Type"
  | Var x -> Pretty.text (Name.to_string x)
  | Con c -> Constant.pretty c
  | Lam b ->
    paren (p > 0) begin
      let (g, t) = unbind_raw b in
      let g = pretty_s (fun a -> pretty 0 a) g in
      Pretty.text "\\ "
      ^^ Pretty.fgrp g
      ^^ Pretty.text "."
      ^^ Pretty.nest 2 (Pretty.break ^^ pretty 0 t)
    end
  | Fun b ->
    paren (p > 0) begin
      let (g, t) = unbind_raw b in
      let g = pretty_s (fun a -> pretty 0 a) g in
      Pretty.text "("
      ^^ Pretty.fgrp g
      ^^ Pretty.text ") ->"
      ^^ Pretty.nest 2 (Pretty.break ^^ pretty 0 t);
    end
  | App (hd, args) ->
    paren (p > 1) begin
      let args =
        List.fold_right args ~init:Pretty.empty
          ~f:(fun arg acc -> pretty 2 arg ^+^ acc)
      in
      Pretty.fgrp
        (pretty 2 hd ^^ Pretty.nest 2 (Pretty.break ^^ args))
    end

let pretty t = pretty 0 t

