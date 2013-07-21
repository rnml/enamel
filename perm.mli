open Std_internal

module type S = sig
  type elt
  type t
  val id : t
  val compose : t -> t -> t
  val inverse : t -> t
  val swap : elt -> elt -> t
  val apply : t -> elt -> elt
end

module Make (Elt:Identifiable) : S with type elt = Elt.t
