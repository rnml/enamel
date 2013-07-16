open Core.Std

(* we want
   (1) typed names, so that [term name] and [type name] are
       distinguishable by the type system,
   (2) the ability to "cast" a name from one type to another.
       This is particularly useful in translations from a source
       language to a target language when you want to preserve
       identifiers,
   (3) a universal name type, so that we may abstract over names
       of arbitrary things.
*)

module type T = sig
  type t with sexp_of
  include Comparable.S with type t := t
  val to_string : t -> string
  val swap : t * t -> t -> t
end

module Univ : T

type 'a t

module type S = sig
  type 'a name
  type a
  type t = a name with of_sexp
  val type_name : t Type.Name.t
  val type_rep : t Type.Rep.t
  val of_string : string -> t
  include T with type t := t
  val to_univ : t -> Univ.t
  val of_univ : Univ.t -> t option
  val cast    : _ name -> t
  val raw : string -> t
  val preferred : t -> t
end
  with type 'a name := 'a t

module Make (X : sig type a val name : string end) :
  S with type a := X.a

module Free_vars_registry : sig
  type 'a computation = Univ.Set.t -> 'a -> Univ.Set.t
  val register : 'a Type.Name.t -> 'a computation -> unit
  val lookup : 'a Type.Name.t -> 'a computation option
end

module Swap_registry : sig
  type 'a computation = Univ.t * Univ.t -> 'a -> 'a
  val register : 'a Type.Name.t -> 'a computation -> unit
  val lookup : 'a Type.Name.t -> 'a computation option
end
