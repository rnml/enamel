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

  module Record : sig
    module type T = sig
      type t
      val name : t Name.t

      type 'a field
      val name_of : 'a field -> string
      val type_of : 'a field -> 'a Rep.t

      type some_field = Field : 'a field -> some_field
      val fields : some_field list
      module type Repr = sig val get : 'a field -> 'a end

      val encode : t -> (module Repr)

      val decode : (module Repr) -> t

PICK UP HERE!  DEFINE A FOLD OVER THE TYPE SO THAT ONE MAY DEFINE GENERIC FUNCTIONS
      module Fold (X : sig
        type result
        val visit : 'a field -> result -> 'a -> result
      end) : sig
        val fold : X.result -> X.result
      end

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
      module type Repr = sig type a val tag : a tag val value : a end
      type t
      val name : t Name.t
      val encode : t -> (module Repr)
      val decode : (module Repr) -> t
    end
    type 'a t = (module T with type t = 'a)
  end

  val same : 'a t -> 'b t -> ('a, 'b) Equal.t option

end

