open Core.Std

module Nm = New_name.Univ

include New_name.Swap_registry

let rec swap : type a. a Type.Rep.t -> a computation = function
  | Type.Rep.Int    -> fun _ x -> x
  | Type.Rep.Char   -> fun _ x -> x
  | Type.Rep.Float  -> fun _ x -> x
  | Type.Rep.String -> fun _ x -> x
  | Type.Rep.Bool   -> fun _ x -> x
  | Type.Rep.Unit   -> fun _ x -> x
  | Type.Rep.Option a -> fun p v -> Option.map v ~f:(swap a p)
  | Type.Rep.List   a -> fun p v -> List.map   v ~f:(swap a p)
  | Type.Rep.Lazy   a -> fun p v -> Lazy.map   v ~f:(swap a p)
  | Type.Rep.Pair (a, b) ->
    fun p (x, y) -> (swap a p x, swap b p y)
  | Type.Rep.Triple (a, b, c) ->
    fun p (x, y, z) -> (swap a p x, swap b p y, swap c p z)
  | Type.Rep.Record r ->
    let module R = (val r : Type.Rep.Record.T with type t = a) in
    fun p r ->
      let rep = R.project r in
      let lookup (type a) (field : a R.field) : a =
        swap (R.Label.type_of field) p (rep.R.lookup field)
      in
      R.inject {R.lookup}
  | Type.Rep.Variant v ->
    let module V = (val v : Type.Rep.Variant.T with type t = a) in
    fun p v ->
      let V.Tagged (tag, arg) = V.project v in
      let arg = swap (V.Label.type_of tag) p arg in
      V.inject (V.Tagged (tag, arg))
  | Type.Rep.Abstract id ->
    match lookup id with
    | Some x -> x
    | None -> failwithf "swap undefined for %s" (Type.Name.name id) ()

