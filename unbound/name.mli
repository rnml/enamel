open Std_internal

(* we want
   (1) typed names, so that [term name] and [type name] are
       kept distinct by the type system,
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
  val freshen : t -> t
  module Perm : Perm.S with type elt := t
end

module Univ : sig
  include T
  val fresh_wrt : t -> fvs:Set.t -> t
end

type 'a t with sexp
val type_rep : 'a Type.Rep.t -> 'a t Type.Rep.t

module type S = sig
  type 'a name
  type a
  type t = a name with of_sexp
  include T with type t := t
  val of_string : string -> t
  val to_univ : t -> Univ.t
  val of_univ : Univ.t -> t option
  val cast    : _ name -> t
  val raw : string -> t
  val preferred : t -> t
  val create : string -> t
  val type_name : t Type.Name.t
  val type_rep : t Type.Rep.t
end
with type 'a name := 'a t

module Make (X : sig
               type a
               val name : string
               val type_rep : a Type.Rep.t
             end) :
  S with type a := X.a

module Registry : sig
  module Free_vars : sig
    module Term : Generic.S
      with type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
    module Pat : Generic.S
      with type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
  end
  module Binders : Generic.S
    with type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
  module Swap : Generic.S
    with type 'a t = Univ.Perm.t -> 'a -> 'a
end
