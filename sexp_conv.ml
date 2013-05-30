open Core.Std

open Or_error.Monad_infix

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
  | Type.Rep.Array a ->
    let a_to_sexp = to_sexp a in
    Array.sexp_of_t a_to_sexp
  | Type.Rep.Lazy a ->
    let a_to_sexp = to_sexp a in
    Lazy.sexp_of_t a_to_sexp
  | Type.Rep.Ref a ->
    let a_to_sexp = to_sexp a in
    Ref.sexp_of_t a_to_sexp
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
        match V.project a with
        | V.Tagged (tag, arg) ->
          Sexp.List [
            Sexp.Atom (V.Label.name_of tag);
            to_sexp (V.Label.type_of tag) arg;
          ]
    end

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
  | Type.Rep.Array a ->
    let a_of_sexp = of_sexp a in
    Array.t_of_sexp a_of_sexp
  | Type.Rep.Lazy a ->
    let a_of_sexp = of_sexp a in
    Lazy.t_of_sexp a_of_sexp
  | Type.Rep.Ref a ->
    let a_of_sexp = of_sexp a in
    Ref.t_of_sexp a_of_sexp
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
    let module R = (val r : Type.Rep.Record.T with type t = a) in
    let map =
      List.map R.Label.all ~f:(function
      | R.Label.Label tag ->
        let of_sexp sexp =

        in
        (R.Label.name_of tag, of_sexp)

      )
      |! String.Map.of_alist_exn
  |!
    in
    fun sexp ->
      with_return (fun {return} ->
        match sexp with
        | Sexp.List entries ->
          List.map entries ~f:(function
          | Sexp.List [Sexp.Atom key; value] -> (key, value)
          | entry ->
            return
              (error "Record.of_sexp entry does not match\
                   \ (ATOM VALUE) pattern" entry Fn.id)
          )
        | Sexp.Atom _ ->
          error "Record.of_sexp expected list but found atom"
            sexp Fn.id
      )
      |! Or_error.ok_exn
      |! String.Map.of_alist
      (*
    begin
      function
    end
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
  | _ -> assert false
      *)