open Core.Std

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

  module Record : sig
    module type T = sig
      type 'a field
      val name_of : 'a field -> string
      val type_of : 'a field -> 'a Rep.t
      type some_field = Field : 'a field -> some_field
      val fields : some_field list
      module type Repr = sig
        val get : 'a field -> 'a
      end
      type t
      val name : t Name.t
      val encode : t -> (module Repr)
      val decode : (module Repr) -> t
    end
    type 'a t = (module T with type t = 'a)
  end

  module Variant : sig
    module type T = sig
      type 'a tag
      val name_of : 'a tag -> string
      val type_of : 'a tag -> 'a Rep.t
      type some_tag = Tag : 'a tag -> some_tag
      val tags : some_tag list
      module type Repr = sig
        type a
        val tag : a tag
        val value : a
      end
      type t
      val name : t Name.t
      val encode : t -> (module Repr)
      val decode : (module Repr) -> t
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

