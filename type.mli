open Core.Std

module Equal :
  (module type of Type_equal with type ('a, 'b) t = ('a, 'b) Type_equal.t)

module Name :
  (module type of Type_equal.Id with type 'a t = 'a Type_equal.Id.t)

(** runtime type representations *)
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

end

