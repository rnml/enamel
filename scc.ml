open Core.Std

(* See "Lazy depth first search and Linear Graph Algorithms in Haskell"
   by King and Launchbury *)

module Make (X : sig
  module Vertex : sig
    type t
    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end
end) = struct

  open X

  module Edge = struct
    type t = Vertex.t * Vertex.t
    let flip (v, w) = (w, v)
  end

  module rec Tree : sig
    type 'a t = Node of 'a * 'a Forest.t
    val post_order : 'a t -> 'a list
  end = struct
    type 'a t = Node of 'a * 'a Forest.t
    let post_order (Node (x, f)) = Forest.post_order f @ [x]
  end

  and Forest : sig
    type 'a t = 'a Tree.t list
    val post_order : 'a t -> 'a list
  end = struct
    type 'a t = 'a Tree.t list
    let post_order ts = List.concat_map ts ~f:Tree.post_order

  end

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

    let outgoing t v = Option.value ~default:[] (Map.find t v)

    let dfs t vs =
      let visited = Vertex.Hash_set.create ~size:(Map.length t) () in
      let rec loop = function
        | [] -> []
        | v :: vs ->
          if Hash_set.mem visited v then
            loop vs
          else begin
            Hash_set.add visited v;
            let ws = outgoing t v in
            let ws = loop ws in
            let vs = loop vs in
            Tree.Node (v, ws) :: vs
          end
      in
      loop vs

    let dff t = dfs t (vertices t)

    let post_order t = Forest.post_order (dff t)

    let scc t = dfs (transpose t) (List.rev (post_order t))

  end

end
