open Core.Std

open Or_error.Monad_infix

module Nm = New_name.Univ

type 'a computation = 'a -> Nm.Set.t -> Nm.Set.t

module Registry = Type.Registry (struct type 'a t = 'a computation end)

let register = Registry.register

let rec fvs_aux : type a. a Type.Rep.t -> a computation = function
  | Type.Rep.Int    -> fun _ acc -> acc
  | Type.Rep.Char   -> fun _ acc -> acc
  | Type.Rep.Float  -> fun _ acc -> acc
  | Type.Rep.String -> fun _ acc -> acc
  | Type.Rep.Bool   -> fun _ acc -> acc
  | Type.Rep.Unit   -> fun _ acc -> acc
  | Type.Rep.Option a ->
    let fvs_a = fvs_aux a in
    fun v acc -> Option.fold ~init:acc ~f:(fun acc a -> fvs_a a acc) v
  | Type.Rep.List a ->
    let fvs_a = fvs_aux a in
    fun v acc -> List.fold ~init:acc ~f:(fun acc a -> fvs_a a acc) v
  | Type.Rep.Array a ->
    let fvs_a = fvs_aux a in
    fun v acc -> Array.fold ~init:acc ~f:(fun acc a -> fvs_a a acc) v
  | Type.Rep.Lazy a ->
    let fvs_a = fvs_aux a in
    fun v acc -> let a = Lazy.force v in fvs_a a acc
  | Type.Rep.Ref a ->
    let fvs_a = fvs_aux a in
    fun v acc -> let a = !v in fvs_a a acc
  | Type.Rep.Pair (a, b) ->
    let fvs_a = fvs_aux a in
    let fvs_b = fvs_aux b in
    fun (x, y) acc ->
      let acc = fvs_a x acc in
      let acc = fvs_b y acc in
      acc
  | Type.Rep.Triple (a, b, c) ->
    let fvs_a = fvs_aux a in
    let fvs_b = fvs_aux b in
    let fvs_c = fvs_aux c in
    fun (x, y, z) acc ->
      let acc = fvs_a x acc in
      let acc = fvs_b y acc in
      let acc = fvs_c z acc in
      acc
  | Type.Rep.Record r ->
    let module R = (val r : Type.Rep.Record.T with type t = a) in
    fun (rcd : R.t) acc ->
      let entry : Nm.Set.t -> R.Label.univ -> Nm.Set.t =
        fun acc (R.Label.Label field) ->
          fvs_aux (R.Label.type_of field) (R.get field rcd) acc
      in
      List.fold ~f:entry ~init:acc R.Label.all
  | Type.Rep.Variant v ->
    let module V = (val v : Type.Rep.Variant.T with type t = a) in
    fun (vnt : V.t) acc ->
      let V.Tagged (tag, arg) = V.project vnt in
      fvs_aux (V.Label.type_of tag) arg acc
  | Type.Rep.Abstract id ->
    match Registry.lookup id with
    | Some x -> x
    | None -> failwithf "no fvs defined for %s" (Type.Name.name id) ()

let fvs ty x = fvs_aux ty x Nm.Set.empty
