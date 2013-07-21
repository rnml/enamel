open Core.Std

module type S = sig
  type 'a t
  val register  : 'a Type.Name.t -> 'a t -> unit
  val lookup : 'a Type.Name.t -> 'a t option
end

module Make (X : T1) : S with type 'a t = 'a X.t

