module Sound = struct
  type t =
    | Roar
    | Meow of int
    | Bark of string * int
  let type_rep =
    Type.Rep.Variant (module struct
      type nonrec t = t
      let name : t Type.Name.t = Type.Name.create ~name:"Sound.t"
      module Label = struct
        type 'a t =
        | Roar : unit t
        | Meow : int t
        | Bark : (string * int) t
        let name_of : type a. a t -> string = function
          | Roar -> "roar"
          | Meow -> "meow"
          | Bark -> "bark"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Roar -> Type.Rep.Unit
          | Meow -> Type.Rep.Int
          | Bark -> Type.Rep.(Pair (String, Int))
        type univ = Label : 'a t -> univ
        let all = [Label Roar; Label Bark; Label Meow]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Roar        -> Tagged (Label.Roar, ())
        | Meow a      -> Tagged (Label.Meow, a)
        | Bark (a, b) -> Tagged (Label.Bark, (a, b))
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Roar, ())     -> Roar
        | (Label.Meow, a)      -> Meow a
        | (Label.Bark, (a, b)) -> Bark (a, b)
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = t)
end

module Animal = struct
  type t = {
    name : string;
    sound : Sound.t;
    size : int;
  }
  let type_rep =
    Type.Rep.Record (module struct
      type nonrec t = t
      let name : t Type.Name.t = Type.Name.create ~name:"Animal.t"
      module Label = struct
        type 'a t =
        | Name  : string t
        | Sound : Sound.t t
        | Size  : int t
        let name_of : type a. a t -> string = function
          | Name  -> "name"
          | Sound -> "sound"
          | Size  -> "size"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Name  -> Type.Rep.String
          | Sound -> Sound.type_rep
          | Size  -> Type.Rep.Int
        type univ = Label : 'a t -> univ
        let all = [Label Name; Label Sound; Label Size]
      end
      type 'a field = 'a Label.t
      type rep = { lookup : 'a. 'a field -> 'a }
      let inject {lookup} = {
        name  = lookup Label.Name;
        sound = lookup Label.Sound;
        size  = lookup Label.Size;
      }
      let get (type a) (f : a field) (t:t) : a =
        match f with
        | Label.Name  -> t.name
        | Label.Sound -> t.sound
        | Label.Size  -> t.size
      let project r = { lookup = fun field -> get field r }
    end : Type.Rep.Record.T with type t = t)
end

module Even_odd_lists = struct

  type even =
    | Nil
    | Cons of int * odd
  and odd = {
    head : int;
    tail : even;
  }

  let rec type_rep_of_even =
    Type.Rep.Variant (module struct
      type nonrec t = even
      let name : t Type.Name.t = Type.Name.create ~name:"Sound.t"
      module Label = struct
        type 'a t =
        | Nil : unit t
        | Cons : (int * odd) t
        let name_of : type a. a t -> string = function
          | Nil -> "nil"
          | Cons -> "cons"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Nil -> Type.Rep.Unit
          | Cons -> Type.Rep.(Pair (Int, type_rep_of_odd))
        type univ = Label : 'a t -> univ
        let all = [Label Nil; Label Cons]
      end
      type 'a tag = 'a Label.t
      type rep = Tagged : 'a tag * 'a -> rep
      let project = function
        | Nil         -> Tagged (Label.Nil, ())
        | Cons (a, b) -> Tagged (Label.Cons, (a, b))
      let put (type a) (tag : a tag) (arg : a) : t =
        match (tag, arg) with
        | (Label.Nil,  ())     -> Nil
        | (Label.Cons, (a, b)) -> Cons (a, b)
      let inject = fun (Tagged (tag, arg)) -> put tag arg
    end : Type.Rep.Variant.T with type t = even)

  and type_rep_of_odd =
    Type.Rep.Record (module struct
      type nonrec t = odd
      let name : t Type.Name.t = Type.Name.create ~name:"Animal.t"
      module Label = struct
        type 'a t =
        | Head : int t
        | Tail : even t
        let name_of : type a. a t -> string = function
          | Head -> "head"
          | Tail -> "tail"
        let type_of : type a. a t -> a Type.Rep.t = function
          | Head -> Type.Rep.Int
          | Tail -> type_rep_of_even
        type univ = Label : 'a t -> univ
        let all = [Label Head; Label Tail]
      end
      type 'a field = 'a Label.t
      type rep = { lookup : 'a. 'a field -> 'a }
      let inject {lookup} = {
        head = lookup Label.Head;
        tail = lookup Label.Tail;
      }
      let get (type a) (f : a field) (t:t) : a =
        match f with
        | Label.Head -> t.head
        | Label.Tail -> t.tail
      let project r = { lookup = fun field -> get field r }
    end : Type.Rep.Record.T with type t = odd)

end
