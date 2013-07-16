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

  module Key = struct
    module T = struct
      type t = Uid.t * Uid.t list with sexp, compare
      let hash (t:t) = Hashtbl.hash t
    end
    include T
    include Hashable.Make (T)
  end

  let table = Key.Table.create ()

  module Make1 (X : T1) : sig
    val lookup : 'a t -> 'a X.t t
  end = struct
    let hd = Uid.create ()
    let lookup t =
      let key = (hd, [uid t]) in
      Obj.magic (Key.Table.find_or_add table key ~default:(fun () -> create ~name:""))
  end

  module Make2 (X : T2) : sig
    val lookup : 'a t -> 'b t -> ('a, 'b) X.t t
  end = struct
    let hd = Uid.create ()
    let lookup a b =
      let key = (hd, [uid a; uid b]) in
      Obj.magic (Key.Table.find_or_add table key ~default:(fun () -> create ~name:""))
  end

  module Make3 (X : T3) : sig
    val lookup : 'a t -> 'b t -> 'c t -> ('a, 'b, 'c) X.t t
  end = struct
    let hd = Uid.create ()
    let lookup a b c =
      let key = (hd, [uid a; uid b; uid c]) in
      Obj.magic (Key.Table.find_or_add table key ~default:(fun () -> create ~name:""))
  end
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

  module Id = struct

    module Option_name = Name.Make1 (struct type 'a t = 'a option end)
    module List_name   = Name.Make1 (struct type 'a t = 'a list end)
    module Lazy_name   = Name.Make1 (struct type 'a t = 'a Lazy.t end)
    module Pair_name   = Name.Make2 (struct type ('a, 'b) t = 'a * 'b end)
    module Triple_name = Name.Make3 (struct type ('a, 'b, 'c) t = 'a * 'b * 'c end)

    let rec id : type a. a t -> a Name.t = function
      | Abstract id -> id
      | Int     -> Name.int
      | Char    -> Name.char
      | Float   -> Name.float
      | String  -> Name.string
      | Bool    -> Name.bool
      | Unit    -> Name.unit
      | Variant x -> let module X = (val x : Rep.Variant.T with type t = a) in X.name
      | Record  x -> let module X = (val x : Rep.Record.T  with type t = a) in X.name
      | Option a         -> Option_name.lookup (id a)
      | List   a         -> List_name.lookup (id a)
      | Lazy   a         -> Lazy_name.lookup (id a)
      | Pair   (a, b)    -> Pair_name.lookup (id a) (id b)
      | Triple (a, b, c) -> Triple_name.lookup (id a) (id b) (id c)
  end

  let rec same t1 t2 = Result.ok (Name.same_witness (id t1) (id t2))

end

