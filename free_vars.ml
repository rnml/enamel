open Core.Std

module Nm = New_name.Univ

include New_name.Free_vars_registry

let rec fv_aux : type a. a Type.Rep.t -> a computation = function
  | Type.Rep.Int    -> fun acc _ -> acc
  | Type.Rep.Char   -> fun acc _ -> acc
  | Type.Rep.Float  -> fun acc _ -> acc
  | Type.Rep.String -> fun acc _ -> acc
  | Type.Rep.Bool   -> fun acc _ -> acc
  | Type.Rep.Unit   -> fun acc _ -> acc
  | Type.Rep.Option a ->
    let fv_a = fv_aux a in
    fun acc v -> Option.fold ~f:fv_a ~init:acc v
  | Type.Rep.List a ->
    let fv_a = fv_aux a in
    fun acc v -> List.fold ~f:fv_a ~init:acc v
  | Type.Rep.Lazy a ->
    fun acc v -> fv_aux a acc (Lazy.force v)
  | Type.Rep.Pair (a, b) ->
    fun acc (x, y) ->
      let acc = fv_aux a acc x in
      let acc = fv_aux b acc y in
      acc
  | Type.Rep.Triple (a, b, c) ->
    fun acc (x, y, z) ->
      let acc = fv_aux a acc x in
      let acc = fv_aux b acc y in
      let acc = fv_aux c acc z in
      acc
  | Type.Rep.Record r ->
    let module R = (val r : Type.Rep.Record.T with type t = a) in
    fun acc r ->
      let entry : Nm.Set.t -> R.Label.univ -> Nm.Set.t =
        fun acc (R.Label.Label field) ->
          fv_aux (R.Label.type_of field) acc (R.get field r)
      in
      List.fold ~f:entry ~init:acc R.Label.all
  | Type.Rep.Variant v ->
    let module V = (val v : Type.Rep.Variant.T with type t = a) in
    fun acc v ->
      let V.Tagged (tag, arg) = V.project v in
      fv_aux (V.Label.type_of tag) acc arg
  | Type.Rep.Abstract id ->
    match lookup id with
    | Some x -> x
    | None -> failwithf "fv undefined for %s" (Type.Name.name id) ()

let fv ty x = fv_aux ty Nm.Set.empty x
