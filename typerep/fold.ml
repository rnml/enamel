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

module Make (X : T) = struct

  type acc = X.acc

  let rec fold : type a. a Type.Rep.t -> a X.t = function
    | Type.Rep.Int    -> fun acc _ -> acc
    | Type.Rep.Char   -> fun acc _ -> acc
    | Type.Rep.Float  -> fun acc _ -> acc
    | Type.Rep.String -> fun acc _ -> acc
    | Type.Rep.Bool   -> fun acc _ -> acc
    | Type.Rep.Unit   -> fun acc _ -> acc
    | Type.Rep.Option a ->
      let fv_a = fold a in
      fun acc v -> Option.fold ~f:fv_a ~init:acc v
    | Type.Rep.List a ->
      let fv_a = fold a in
      fun acc v -> List.fold ~f:fv_a ~init:acc v
    | Type.Rep.Lazy a ->
      fun acc v -> fold a acc (Lazy.force v)
    | Type.Rep.Pair (a, b) ->
      fun acc (x, y) ->
        let acc = fold a acc x in
        let acc = fold b acc y in
        acc
    | Type.Rep.Triple (a, b, c) ->
      fun acc (x, y, z) ->
        let acc = fold a acc x in
        let acc = fold b acc y in
        let acc = fold c acc z in
        acc
    | Type.Rep.Record r ->
      let module R = (val r : Type.Rep.Record.T with type t = a) in
      fun acc r ->
        let entry : X.acc -> R.Label.univ -> X.acc =
          fun acc (R.Label.Label field) ->
            fold (R.Label.type_of field) acc (R.get field r)
        in
        List.fold ~f:entry ~init:acc R.Label.all
    | Type.Rep.Variant v ->
      let module V = (val v : Type.Rep.Variant.T with type t = a) in
      fun acc v ->
        let V.Tagged (tag, arg) = V.project v in
        fold (V.Label.type_of tag) acc arg
    | Type.Rep.Abstract id ->
      match X.lookup id with
      | Some x -> x
      | None ->
        failwithf "%s undefined for %s"
          X.generic_name
          (Type.Name.name id)
          ()
end
