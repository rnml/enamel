open Core.Std

module Systemf : sig

  module Kind : sig
    type t = Star | Arr of t * t with sexp
    val type_rep : t Type.Rep.t
  end

end
