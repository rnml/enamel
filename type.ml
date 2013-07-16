open Core.Std

module Equal = Type_equal

module Name = struct
  include Type_equal.Id
  let int    : int    t = create ~name:"int"
  let char   : char   t = create ~name:"char"
  let float  : float  t = create ~name:"float"
  let string : string t = create ~name:"string"
  let bool   : bool   t = create ~name:"bool"
  let unit   : unit   t = create ~name:"unit"
end

module Registry (Data : sig type 'a t end) = struct

  type entry = Entry : 'a Name.t * 'a Data.t -> entry

  let t = Name.Uid.Table.create ~size:10 ()

  let register name data =
    let key = Name.uid name in
    let data = Entry (name, data) in
    Hashtbl.set t ~key ~data

  module Lift = Type_equal.Lift (Data)

  let lookup (type a) (name : a Name.t) : a Data.t option =
    Option.map (Hashtbl.find t (Name.uid name)) ~f:(function
    | Entry (name', data) ->
      let eq = Name.same_witness_exn name' name in
      Type_equal.conv (Lift.lift eq) data)

end

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
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
    | Record of 'a Rep.Record.t
    | Variant of 'a Rep.Variant.t
    | Abstract of 'a Name.t

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
      val get : 'a field -> t -> 'a
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
      val put : 'a tag -> 'a -> t
    end
    type 'a t = (module T with type t = 'a)
  end

  val id : 'a t -> 'a Name.t
  val same : 'a t -> 'b t -> ('a, 'b) Equal.t option

end = struct

  include Rep

(*
  let id : type a. a t -> a Name.t = function
    | Abstract (id, _) -> id
    | Int     -> Name.int
    | Char    -> Name.char
    | Float   -> Name.float
    | String  -> Name.string
    | Bool    -> Name.bool
    | Unit    -> Name.unit
    | Variant x ->
      let module X = (val x : Rep.Variant.T with type t = a) in
      X.name
    | Record x ->
      let module X = (val x : Rep.Record.T with type t = a) in
      X.name
    | Option _ -> assert false
    | List   _ -> assert false
    | Array  _ -> assert false
    | Lazy   _ -> assert false
    | Pair   _ -> assert false
    | Triple _ -> assert false
*)

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
    | (Abstract id1, Abstract id2) ->
      Result.ok (Name.same_witness id1 id2)
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
    | (Record _, _) -> None
    | (Variant _, _) -> None
    | (Pair _, _) -> None
    | (Triple _, _) -> None
    | (Abstract _, _) -> None
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

