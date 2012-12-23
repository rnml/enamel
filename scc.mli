open Core.Std

module Graph (Vertex : sig
  type t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end) : sig
  module Edge : sig
    type t = Vertex.t * Vertex.t
    val flip : t -> t
  end
  module rec Tree : sig
    type 'a t = Node of 'a * 'a Forest.t
    val post_order : 'a t -> 'a list
    val pre_order : 'a t -> 'a list
  end and Forest : sig
    type 'a t = 'a Tree.t list
    val post_order : 'a t -> 'a list
    val pre_order : 'a t -> 'a list
  end
  module Graph : sig
    type t
    val vertices   : t -> Vertex.t list
    val edges      : t -> Edge.t list
    val build      : Edge.t list -> t
    val transpose  : t -> t
    val out_degree : t -> int Vertex.Map.t
    val in_degree  : t -> int Vertex.Map.t
    val outgoing   : t -> Vertex.t -> Vertex.t list
    val dfs : t -> Vertex.t list -> Vertex.t Forest.t
    val dff : t -> Vertex.t Forest.t
    val scc : t -> Vertex.t Forest.t
  end
  val scc : Edge.t list -> Vertex.t list list (* simple interface version *)
end

module Make (Vertex : sig
  type t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end) : sig
  val scc : (Vertex.t * Vertex.t) list -> Vertex.t list list
end
