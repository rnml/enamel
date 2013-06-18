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

module Even_odd_lists : sig
  type even =
  | Nil
  | Cons of int * odd
  and odd = {
    head : int;
    tail : even;
  }
  val type_rep_of_even : even Type.Rep.t
  val type_rep_of_odd : odd Type.Rep.t
end
