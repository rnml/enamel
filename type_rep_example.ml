module Sound = struct
  type t = Roar | Meow of int | Bark of string * int
  let type_rep =
    let module T = struct
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

    end in
    Type.Rep.Variant (module T : Type.Rep.Variant.T with type t = t)
end

module Animal = struct
  type t = {
    name : string;
    sound : Sound.t;
    size : int;
  }
  let type_rep =
    let module T = struct
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

    end in
    Type.Rep.Record (module T : Type.Rep.Record.T with type t = t)
end
