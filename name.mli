open Std_internal

module type T = sig
  type t with sexp

  include Comparable.S with type t := t

  val map_domain : _ Map.t -> Set.t

  val dummy : t
  val raw : string -> t
  val succ : t -> t
  val preferred : t -> t
  val freshen : t -> t
  val next : t -> not_in:Set.t -> t

  val to_string : t -> string
  val of_string : string -> t
  val pretty : t -> Pretty.doc
  val swap : t * t -> t -> t
end

include T

module type S = sig
  include T
  type name
  val to_name : t -> name
  val of_name : name -> t
end
  with type name := t

module Make (X : sig end) : S


