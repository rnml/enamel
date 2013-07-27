open Core.Std

module type T = sig
  type acc
  val generic_name : string
  include Generic.S with type 'a t = acc -> 'a -> acc
end

module type S = sig
  type acc
  val fold : 'a Type.Rep.t -> acc -> 'a -> acc
end

module Make (X : T) : S with type acc = X.acc
