open Core.Std

open Or_error.Monad_infix

module To_sexp_registry =
  Generic.Make (struct type 'a t = 'a -> Sexp.t end)

let register_to_sexp = To_sexp_registry.register

let rec to_sexp : type a. a Type.Rep.t -> a -> Sexp.t = function
  | Type.Rep.Int    -> Int.sexp_of_t
  | Type.Rep.Char   -> Char.sexp_of_t
  | Type.Rep.Float  -> Float.sexp_of_t
  | Type.Rep.String -> String.sexp_of_t
  | Type.Rep.Bool   -> Bool.sexp_of_t
  | Type.Rep.Unit   -> Unit.sexp_of_t
  | Type.Rep.Option a ->
    let a_to_sexp = to_sexp a in
    Option.sexp_of_t a_to_sexp
  | Type.Rep.List a ->
    let a_to_sexp = to_sexp a in
    List.sexp_of_t a_to_sexp
  | Type.Rep.Lazy a ->
    let a_to_sexp = to_sexp a in
    Lazy.sexp_of_t a_to_sexp
  | Type.Rep.Pair (a, b) ->
    let a_to_sexp = to_sexp a in
    let b_to_sexp = to_sexp b in
    Tuple.T2.sexp_of_t a_to_sexp b_to_sexp
  | Type.Rep.Triple (a, b, c) ->
    let a_to_sexp = to_sexp a in
    let b_to_sexp = to_sexp b in
    let c_to_sexp = to_sexp c in
    Tuple.T3.sexp_of_t a_to_sexp b_to_sexp c_to_sexp
  | Type.Rep.Record r ->
    let module R = (val r : Type.Rep.Record.T with type t = a) in
    let entry : R.Label.univ -> R.t -> Sexp.t = function
      | R.Label.Label field ->
        let name = Sexp.Atom (R.Label.name_of field) in
        let val_to_sexp = to_sexp (R.Label.type_of field) in
        fun r ->
          Sexp.List [
            name;
            val_to_sexp ((R.project r).R.lookup field);
          ]
    in
    let entries = List.map R.Label.all ~f:entry in
    fun a ->
      Sexp.List (List.map entries ~f:(fun f -> f a))
  | Type.Rep.Variant v ->
    let module V = (val v : Type.Rep.Variant.T with type t = a) in
    begin
      fun a ->
        let V.Tagged (tag, arg) = V.project a in
        Sexp.List [
          Sexp.Atom (V.Label.name_of tag);
          to_sexp (V.Label.type_of tag) arg;
        ]
    end
  | Type.Rep.Abstract id ->
    match To_sexp_registry.lookup id with
    | Some x -> x
    | None ->
      failwithf "no to_sexp defined for %s" (Type.Name.name id) ()

module Of_sexp_registry =
  Generic.Make (struct type 'a t = Sexp.t -> 'a end)

let register_of_sexp = Of_sexp_registry.register

let rec of_sexp : type a. a Type.Rep.t -> Sexp.t -> a = function
  | Type.Rep.Int    -> Int.t_of_sexp
  | Type.Rep.Char   -> Char.t_of_sexp
  | Type.Rep.Float  -> Float.t_of_sexp
  | Type.Rep.String -> String.t_of_sexp
  | Type.Rep.Bool   -> Bool.t_of_sexp
  | Type.Rep.Unit   -> Unit.t_of_sexp
  | Type.Rep.Option a ->
    let a_of_sexp = of_sexp a in
    Option.t_of_sexp a_of_sexp
  | Type.Rep.List a ->
    let a_of_sexp = of_sexp a in
    List.t_of_sexp a_of_sexp
  | Type.Rep.Lazy a ->
    let a_of_sexp = of_sexp a in
    Lazy.t_of_sexp a_of_sexp
  | Type.Rep.Pair (a, b) ->
    let a_of_sexp = of_sexp a in
    let b_of_sexp = of_sexp b in
    Tuple.T2.t_of_sexp a_of_sexp b_of_sexp
  | Type.Rep.Triple (a, b, c) ->
    let a_of_sexp = of_sexp a in
    let b_of_sexp = of_sexp b in
    let c_of_sexp = of_sexp c in
    Tuple.T3.t_of_sexp a_of_sexp b_of_sexp c_of_sexp
  | Type.Rep.Record r ->
    begin
      let module R = (val r : Type.Rep.Record.T with type t = a) in
      let fields =
        String.Map.of_alist_exn
          (List.map R.Label.all ~f:(function
          | R.Label.Label tag -> (R.Label.name_of tag, ())))
      in
      function
      | Sexp.Atom _ as sexp ->
        failwiths "Record.of_sexp expected list but found atom"
          sexp Fn.id
      | Sexp.List entries ->
        List.map entries ~f:(function
        | Sexp.List [Sexp.Atom key; value] -> (key, value)
        | entry ->
          failwiths "Record.of_sexp entry does not match\
                  \ (ATOM VALUE) pattern" entry Fn.id)
        |! String.Map.of_alist
        |! function
          | `Duplicate_key key ->
            failwiths "Record.of_sexp duplicate key"
              key String.sexp_of_t
          | `Ok entries ->
            Map.merge fields entries ~f:(fun ~key data ->
              match data with
              | `Both ((), sexp) -> Some sexp
              | `Left () ->
                failwiths "missing field" key String.sexp_of_t
              | `Right sexp ->
                failwiths "unknown field" (key, sexp)
                <:sexp_of< string * Sexp.t >>)
            |! fun map ->
              R.inject
                { R.lookup = fun field ->
                  let key = R.Label.name_of field in
                  let typ = R.Label.type_of field in
                  match Map.find map key with
                  | Some sexp -> of_sexp typ sexp
                  | None ->
                    failwiths "missing field" key String.sexp_of_t
                }
    end
  | Type.Rep.Variant v ->
    begin
      let module V = (val v : Type.Rep.Variant.T with type t = a) in
      let tags =
        String.Map.of_alist_exn
          (List.map V.Label.all ~f:(function
          | V.Label.Label tag as univ ->
            (V.Label.name_of tag, univ)))
      in
      function
      | Sexp.List [Sexp.Atom tag; arg] ->
        begin
          match Map.find tags tag with
          | None ->
            failwiths "unknown tag" (tag, arg)
            <:sexp_of< string * Sexp.t >>
          | Some (V.Label.Label tag) ->
            let arg = of_sexp (V.Label.type_of tag) arg in
            V.inject (V.Tagged (tag, arg))
        end
      | sexp ->
        failwiths "Variant.of_sexp expected (ATOM ARG) but found \
                   atom" sexp Fn.id
    end
  | Type.Rep.Abstract id ->
    match Of_sexp_registry.lookup id with
    | Some x -> x
    | None ->
      failwithf "no of_sexp defined for %s" (Type.Name.name id) ()
