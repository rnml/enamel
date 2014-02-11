open Core.Std

module Equal :
  module type of Type_equal
  with type ('a, 'b) t = ('a, 'b) Type_equal.t

module Name : sig
  include module type of Type_equal.Id
  with type 'a t = 'a Type_equal.Id.t
  module Make1 (X : sig
                  val name : string
                  type 'a t
                end) : sig
    val lookup : 'a t -> 'a X.t t
  end
  module Make2 (X : sig
                  val name : string
                  type ('a, 'b) t
                end) : sig
    val lookup : 'a t -> 'b t -> ('a, 'b) X.t t
  end
  module Make3 (X : sig
                  val name : string
                  type ('a, 'b, 'c) t
                end) : sig
    val lookup : 'a t -> 'b t -> 'c t -> ('a, 'b, 'c) X.t t
  end
end

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
    | Lazy : 'a t -> 'a Lazy.t t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
    | Record of 'a Rep.Record.t
    | Variant of 'a Rep.Variant.t
    | Abstract of 'a Name.t

  val id : 'a t -> 'a Name.t
  val same : 'a t -> 'b t -> ('a, 'b) Equal.t option

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
        with type rep := rep
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
        with type rep := rep
      val put : 'a tag -> 'a -> t
    end
    type 'a t = (module T with type t = 'a)
  end

end

