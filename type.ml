open Core.Std
open Core_extended.Std

module Equal = Type_equal
module Name = Type_equal.Id

module rec Rep : sig

  type 'a t =
    | Int : int t
    | Char : char t
    | Float : float t
    | String : string t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | List : 'a t -> 'a list t
    | Array : 'a t -> 'a array t
    | Lazy : 'a t -> 'a Lazy.t t
    | Ref : 'a t -> 'a ref t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
    | Record of 'a Rep.Record.t
    | Variant of 'a Rep.Variant.t

  module type Labeled = sig
    type t
    val name : t Name.t
    module Label : sig
      type 'a t
      val name_of : 'a t -> string
      val type_of : 'a t -> 'a Rep.t
      type univ = Label : 'a t -> univ
      val all : univ list
    end
    type rep
    val inject : rep -> t
    val project : t -> rep
  end

  module Record : sig
    module type T = sig
      type 'a field
      type rep = { lookup : 'a. 'a field -> 'a }
      include Labeled
        with type 'a Label.t = 'a field
         and type rep := rep
    end
    type 'a t = (module T with type t = 'a)
  end

  module Variant : sig
    module type T = sig
      type 'a tag
      type rep = Tagged : 'a tag * 'a -> rep
      include Labeled
        with type 'a Label.t = 'a tag
         and type rep := rep
    end
    type 'a t = (module T with type t = 'a)
  end

  val same : 'a t -> 'b t -> ('a, 'b) Equal.t option

end = struct

  include Rep

  let rec same : type a b. a t -> b t -> (a, b) Equal.t option =
  fun t1 t2 ->
    match (t1, t2) with
    | (Int, Int) -> Some Equal.refl
    | (Char, Char) -> Some Equal.refl
    | (Float, Float) -> Some Equal.refl
    | (String, String) -> Some Equal.refl
    | (Bool, Bool) -> Some Equal.refl
    | (Unit, Unit) -> Some Equal.T
    | (Option e1, Option e2) -> begin
      match same e1 e2 with
      | None -> None
      | Some Type_equal.T -> Some Type_equal.T
    end
    | (List a1, List a2) -> begin
      match same a1 a2 with
      | None -> None
      | Some Type_equal.T -> Some Type_equal.T
    end
    | (Array a1, Array a2) -> begin
      match same a1 a2 with
      | None -> None
      | Some Type_equal.T -> Some Type_equal.T
    end
    | (Lazy a1, Lazy a2) -> begin
      match same a1 a2 with
      | None -> None
      | Some Type_equal.T -> Some Type_equal.T
    end
    | (Ref a1, Ref a2) -> begin
      match same a1 a2 with
      | None -> None
      | Some Type_equal.T -> Some Type_equal.T
    end
    | (Pair (a1, b1), Pair (a2, b2)) -> begin
      match (same a1 a2, same b1 b2) with
      | (None, _) -> None
      | (_, None) -> None
      | (Some Type_equal.T, Some Type_equal.T) -> Some Type_equal.T
    end
    | (Triple (a1, b1, c1), Triple (a2, b2, c2)) -> begin
      match (same a1 a2, same b1 b2, same c1 c2) with
      | (None, _, _) -> None
      | (_, None, _) -> None
      | (_, _, None) -> None
      | ( Some Type_equal.T
            , Some Type_equal.T
              , Some Type_equal.T) -> Some Type_equal.T
    end
    | (Record r1, Record r2) ->
        let module R1 = (val r1 : Rep.Record.T with type t = a) in
        let module R2 = (val r2 : Rep.Record.T with type t = b) in
        Name.same_witness R1.name R2.name |> Result.ok
    | (Variant r1, Variant r2) ->
        let module R1 = (val r1 : Rep.Variant.T with type t = a) in
        let module R2 = (val r2 : Rep.Variant.T with type t = b) in
        Name.same_witness R1.name R2.name |> Result.ok
    | (Int, _) -> None
    | (Char, _) -> None
    | (Float, _) -> None
    | (String, _) -> None
    | (Bool, _) -> None
    | (Unit, _) -> None
    | (Option _, _) -> None
    | (List _, _) -> None
    | (Array _, _) -> None
    | (Lazy _, _) -> None
    | (Ref _, _) -> None
    | (Record _, _) -> None
    | (Variant _, _) -> None
    | (Pair _, _) -> None
    | (Triple _, _) -> None
  ;;

  module Short_circuit_same : sig
    val same : 'a t -> 'b t -> ('a, 'b) Equal.t option
  end = struct
    let same (type a) (type b) (ty1 : a t) (ty2 : b t) =
      if phys_equal ty1 (Obj.magic ty2)
      then Some (Obj.magic Equal.T)
      else same ty1 ty2
  end
  include Short_circuit_same

end

module Code = struct
  type t =
    | Int
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option of t
    | List of t
    | Array of t
    | Lazy of t
    | Ref of t
    | Pair of t * t
    | Triple of t * t * t
    | Record of (string * t) list
    | Variant of (string * t) list
    | Name of string
  with sexp

  let rec fvs =
    let (+) = String.Set.union in
    function
    | Int | Char | Float | String | Bool | Unit -> String.Set.empty
    | Option t | List t | Array t | Lazy t | Ref t -> fvs t
    | Pair (t1, t2) -> fvs t1 + fvs t2
    | Triple (t1, t2, t3) -> fvs t1 + fvs t2 + fvs t3
    | Record alist
    | Variant alist ->
      List.fold alist ~init:String.Set.empty
        ~f:(fun acc (_, t) -> acc + fvs t)
    | Name x -> String.Set.singleton x

  module Scc = Scc.Make (String)

  module Text = Text_block

  let hwords ws = Text.hcat ~sep:(Text.hstrut 1) ws

  let rec render = function
    | Int    -> Text.text "int"
    | Char   -> Text.text "char"
    | Float  -> Text.text "float"
    | String -> Text.text "string"
    | Bool   -> Text.text "bool"
    | Unit   -> Text.text "unit"
    | Option t -> hwords [render t; Text.text "option"]
    | List t   -> hwords [render t; Text.text "list"]
    | Array t  -> hwords [render t; Text.text "array"]
    | Lazy t   -> hwords [render t; Text.text "Lazy.t"]
    | Ref t    -> hwords [render t; Text.text "rev"]
    | Pair (a, b) ->
      Text.hcat (
        [Text.text "("]
        @ List.intersperse ~sep:(Text.text " * ")
          [render a; render b]
        @ [Text.text ")"]
      )
    | Triple (a, b, c) ->
      Text.hcat (
        [Text.text "("]
        @ List.intersperse ~sep:(Text.text " * ")
          [render a; render b; render c]
        @ [Text.text ")"]
      )
    | Name x -> Text.text x
    | Variant alist ->
      Text.vcat (List.map alist ~f:(fun (c, arg) ->
        hwords [
          Text.text "|";
          Text.text c;
          Text.text "of";
          render arg
        ]))
    | Record alist ->
      Text.vcat [
        Text.text "{";
        Text.hcat [
          Text.hstrut 2;
          Text.vcat (List.map alist ~f:(fun (f, fty) ->
            hwords
              [Text.text f; Text.hcat [render fty; Text.text ";"]]
          ));
        ];
        Text.text "}";
      ]

  let codegen alist =
    let map = String.Map.of_alist_exn alist in
    let edges =
      List.concat_map alist ~f:(fun (x, t) ->
        let ys = Set.to_list (fvs t) in
        List.map ys ~f:(fun y -> (x, y)))
    in
    Text.vcat
      (List.map (Scc.scc edges) ~f:(fun group ->
        Text.vcat
          (List.map group ~f:(fun x ->
            let t : t = Map.find_exn map x in
            hwords [
              Text.text "type";
              Text.text x;
              Text.text "=";
              render t;
            ]
           ))
       ))

end
