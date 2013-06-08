open Core.Std

module Make (Vertex : sig
  type t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end) : sig
  val scc : (Vertex.t * Vertex.t) list -> Vertex.t list list
end
