open Core.Std

module Make (X : sig
  module Vertex : sig
    type t
    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end
end) : sig
  open X
  module Edge : sig
    type t = Vertex.t * Vertex.t
    val flip : t -> t
  end
  module rec Tree : sig
    type 'a t = Node of 'a * 'a Forest.t
  end and Forest : sig
    type 'a t = 'a Tree.t list
  end
  module Graph : sig
    type t
    val vertices   : t -> Vertex.t list
    val edges      : t -> Edge.t list
    val build      : Edge.t list -> t
    val transpose  : t -> t
    val out_degree : t -> int Vertex.Map.t
    val in_degree  : t -> int Vertex.Map.t
    val dfs : t -> Vertex.t list -> Vertex.t Forest.t
    val dff : t -> Vertex.t Forest.t
    val scc : t -> Vertex.t Forest.t
  end
end
