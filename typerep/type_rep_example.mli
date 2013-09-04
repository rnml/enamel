module Dollars : sig
  type t
  val type_rep : t Type.Rep.t
  val of_int : int -> t
end

module Sound : sig
  type t = Roar | Meow of Dollars.t | Bark of string * int
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

module Tree : sig
  type t =
    | Empty
    | Node of t * int * t
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
