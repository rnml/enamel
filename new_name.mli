open Std_internal

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

type 'a t with sexp

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

module Registry : sig
  module Free_vars : sig
    module Term : Generic.S
      with type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
    module Pat : Generic.S
      with type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
  end
  module Swap : Generic.S
    with type 'a t = Univ.t * Univ.t -> 'a -> 'a
end
