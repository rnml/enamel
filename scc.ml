open Core.Std

(* See "Lazy depth first search and Linear Graph Algorithms in Haskell"
   by King and Launchbury *)

module Make (X : sig
  module Vertex : Comparable.S
end) = struct

  open X

  module Edge = struct
    type t = Vertex.t * Vertex.t
    let flip (v, w) = (w, v)
  end

  module rec Tree : sig
    type 'a t = Node of 'a * 'a Forest.t Lazy.t
  end = Tree

  and Forest : sig
    type 'a t = 'a Tree.t list
  end = Forest

  module Graph = struct
    type t = Vertex.t list Vertex.Map.t

    let vertices = Map.keys

    let edges t =
      let open List.Monad_infix in
      Map.to_alist t >>= fun (v, ws) -> ws >>| fun w -> (v, w)

    let build = Vertex.Map.of_alist_multi

    let transpose t = edges t |! List.map ~f:Edge.flip |! build

    let out_degree = Map.map ~f:List.length
    let in_degree t = out_degree (transpose t)

    let dfs _ = assert false

  end

end
