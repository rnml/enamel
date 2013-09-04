open Std_internal

module type S = sig
  type elt
  type t
  val id : t
  val compose : t -> t -> t
  val inverse : t -> t
  val swap : elt -> elt -> t
  val apply : t -> elt -> elt
  val of_alist : (elt, elt) List.Assoc.t -> t
end

module Make (Elt : Identifiable) = struct

  type elt = Elt.t

  type t = {
    map : elt Elt.Map.t;
    inv : elt Elt.Map.t;
  }

  let id = {map = Elt.Map.empty; inv = Elt.Map.empty}

  let inverse t = {map = t.inv; inv = t.map}

  let swap a b =
    if Elt.equal a b then id else begin
      let map = Elt.Map.of_alist_exn [(a, b); (b, a)] in
      {map; inv = map}
    end

  let apply t elt = Option.value ~default:elt (Map.find t.map elt)

  let compose t_snd t_fst =
    Map.merge t_fst.inv t_snd.map ~f:(fun ~key:mid data ->
      let map a ~to_:b =
        if Elt.equal a b then None else Some (a, b)
      in
      match data with
      | `Both (src, tgt) -> map src ~to_:tgt
      | `Left src -> map src ~to_:mid
      | `Right tgt -> map mid ~to_:tgt
    )
    |> Map.fold ~init:id ~f:(fun ~key:_ ~data:(src, tgt) t ->
      { map = Map.add t.map ~key:src ~data:tgt;
        inv = Map.add t.inv ~key:tgt ~data:src; })

  let of_alist xys =
    List.fold xys ~init:id ~f:(fun {map; inv} (src, tgt) ->
      let map =
        match Map.find inv tgt with
        | None -> map
        | Some src' -> Map.remove map src'
      in
      let inv =
        match Map.find map src with
        | None -> inv
        | Some tgt' -> Map.remove inv tgt'
      in
      { map = Map.add map ~key:src ~data:tgt;
        inv = Map.add inv ~key:tgt ~data:src; })
end
