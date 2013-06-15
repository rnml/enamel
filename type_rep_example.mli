module Sound : sig
  type t = Roar | Meow of int | Bark of string * int
  val type_rep : t Type.Rep.t
end

module Animal : sig
  type t = {
    name : string;
    sound : Sound.t;
    size : int;
  }
  val type_rep : t Type.Rep.t
end
