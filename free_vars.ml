open Core.Std

open Or_error.Monad_infix

module Nm = New_name.Univ

type 'a computation = 'a -> Nm.Set.t

let register = New_name.Free_vars_registry.register

let rec fvs_aux : type a. a Type.Rep.t -> Nm.Set.t -> a computation = function
  | Type.Rep.Int    -> fun acc _ -> acc
  | Type.Rep.Char   -> fun acc _ -> acc
  | Type.Rep.Float  -> fun acc _ -> acc
  | Type.Rep.String -> fun acc _ -> acc
  | Type.Rep.Bool   -> fun acc _ -> acc
  | Type.Rep.Unit   -> fun acc _ -> acc
  | Type.Rep.Option a ->
    let fvs_a = fvs_aux a in
    fun acc v -> Option.fold ~f:fvs_a ~init:acc v
  | Type.Rep.List a ->
    let fvs_a = fvs_aux a in
    fun acc v -> List.fold ~f:fvs_a ~init:acc v
  | Type.Rep.Array a ->
    let fvs_a = fvs_aux a in
    fun acc v -> Array.fold ~f:fvs_a ~init:acc v
  | Type.Rep.Lazy a ->
    let fvs_a = fvs_aux a in
    fun acc v -> fvs_a acc (Lazy.force v)
  | Type.Rep.Ref a ->
    let fvs_a = fvs_aux a in
    fun acc v -> fvs_a acc !v
  | Type.Rep.Pair (a, b) ->
    let fvs_a = fvs_aux a in
    let fvs_b = fvs_aux b in
    fun acc (x, y) ->
      let acc = fvs_a acc x in
      let acc = fvs_b acc y in
      acc
  | Type.Rep.Triple (a, b, c) ->
    let fvs_a = fvs_aux a in
    let fvs_b = fvs_aux b in
    let fvs_c = fvs_aux c in
    fun acc (x, y, z) ->
      let acc = fvs_a acc x in
      let acc = fvs_b acc y in
      let acc = fvs_c acc z in
      acc
  | Type.Rep.Record r ->
    let module R = (val r : Type.Rep.Record.T with type t = a) in
    fun acc (rcd : R.t) ->
      let entry : Nm.Set.t -> R.Label.univ -> Nm.Set.t =
        fun acc (R.Label.Label field) ->
          fvs_aux (R.Label.type_of field) acc (R.get field rcd)
      in
      List.fold ~f:entry ~init:acc R.Label.all
  | Type.Rep.Variant v ->
    let module V = (val v : Type.Rep.Variant.T with type t = a) in
    fun acc (vnt : V.t) ->
      let V.Tagged (tag, arg) = V.project vnt in
      fvs_aux (V.Label.type_of tag) acc arg
  | Type.Rep.Abstract id ->
    match New_name.Free_vars_registry.lookup id with
    | Some x -> x
    | None -> failwithf "no fvs defined for %s" (Type.Name.name id) ()

let fvs ty x = fvs_aux ty Nm.Set.empty x
