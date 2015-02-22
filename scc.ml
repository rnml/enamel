open Core.Std

module Make (Vertex : sig
               type t
               include Comparable.S with type t := t
               include Hashable.S with type t := t
             end) = struct
  include Graph_lib.Graph.Make (Vertex)
  let scc es =
    List.map es ~f:(fun (src, dst) -> {Edge. src; dst})
    |> of_edges
    |> scc
    |> List.map ~f:Tree.pre_order
end
