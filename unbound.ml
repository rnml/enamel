open Core.Std

(*
type exp
type pat

module Mode = struct
  type 'm t =
    | Exp of (exp, 'm) Type.Equal.t
    | Pat of (pat, 'm) Type.Equal.t
end

module Alpha = struct

  module Pat = struct
    type 'a t = {
      find_index : ?acc:int -> 'a -> int -> Any_name.t option * int;
      find_name  : ?acc:int -> 'a -> Any_name.t -> int option * int;
    }
  end

  module Exp = struct
    type 'a t = unit
(*
    type 'a t = {
      something_to_help_define_fvs : unit
    }
*)
  end

  module Mode = struct
    type ('a, 'm) t =
      | Exp of (exp, 'm) Type.Equal.t * 'a Pat.t
      | Pat of (pat, 'm) Type.Equal.t * 'a Exp.t
  end

  type ('a, 'm) t = {
    mode : unit -> ('a, 'm) Mode.t;
    abs  : 'p. 'p Pat.t -> int -> 'p -> 'a -> 'a;
    sub  : 'p. 'p Pat.t -> int -> 'p -> 'a -> 'a;
    aeq  : 'a -> 'a -> bool;
    (*fvs : 'a -> 'a *)
  }

  let now thunk = {
    mode = (fun a         -> (Lazy.force thunk).mode a);
    abs  = (fun a b c d e -> (Lazy.force thunk).abs a b c d e);
    sub  = (fun a b c d e -> (Lazy.force thunk).sub a b c d e);
    aeq  = (fun a b       -> (Lazy.force thunk).aeq a b);
  }

end

module Name = struct

  type 'a t =
    | Free of Any_name.t
    | Bound of int * int (* pattern index * variable index within pattern *)

  (* NOTE: the [Bound] case never escapes this file *)

  let dummy = Free Any_name.dummy

  let raw x = Free (Any_name.raw x)

  let pretty = function
    | Free x -> Any_name.pretty x
    | Bound _ -> assert false

  let tc (type m) (mode : m Mode.t) =
    let abs ptc l mode p = function
      | Bound (j, i) -> Bound (j, i)
      | Free x ->
        match mode with
        | `Pat -> Free x
        | `Exp ->
          match fst (ptc.Alpha.Pat.find_name p x) with
          | Some i -> Bound (l, i)
          | None -> Free x
    in
    let sub ptc l mode p = function
      | Free x -> Free x
      | Bound (j, k) ->
        match mode with
        | `Pat -> assert false
        | `Exp ->
          if Int.eq j l then
            match fst (ptc.Alpha.Pat.find_index p k) with
            | Some x -> Free x
            | None -> Bound (j, k)
          else
            Bound (j, k)
    in
    let aeq t1 t2 =
      match (t1, t2) with
      | (Bound (i1, j1), Bound (i2, j2)) -> Int.eq i1 i2 && Int.eq j1 j2
      | (Free x1, Free x2) -> Any_name.equal x1 x2
      | _ -> false
    in
    match mode with
    | Mode.Pat eq ->
      let find_index ?(acc = 0) t i =
        match t with
        | Free x when Int.eq i acc -> (Some x, acc + 1)
        | Free _ -> (None, acc + 1)
        | Bound _ -> (None, acc)
      in
      let find_name ?(acc = 0) t x =
        match t with
        | Free y when Any_name.eq x y -> (Some acc, acc + 1)
        | Free _ -> (None, acc + 1)
        | Bound _ -> (None, acc)
      in
      let mode () = (mode, `Pat {Alpha.Pat.find_index; find_name}) in
      let module Lift =
        Type.Equal.Lift (struct
          type 'a t = 'a Alpha.Pat.t
        end)
      in
      Type.Equal.cast (Lift.f (eq))
        {Alpha.mode; abs; sub; aeq}
    | Mode.Exp _eq ->
      let mode () = (mode, `Exp ()) in
      {Alpha.mode; abs; sub; aeq}

end

(*
module Names = struct
end
*)

(*
module T2 = struct
  let tc a1 a2 =
    let both f =
      lazy begin
        match f a1 with
        | None -> None
        | Some p1 ->
          match f a2 with
          | None -> assert false
          | Some p2 -> Some (p1, p2)
      end
    in
    let pat () =
      match both (fun a -> a.pat ()) with
      | None -> None
      | Some (p1, p2) ->
        let find_index ?(acc = 0) (x1, x2) i =
          match p1.Alpha.Pat.find_index ~acc x1 i with
          | (Some j, acc) -> (Some j, acc)
          | (None, acc) ->
            (* TODO *)
        in
        let find_name ?(acc = 0) t x =
          match t with
          | Free y when Any_name.eq x y -> (Some acc, acc + 1)
          | Free _ -> (None, acc + 1)
          | Bound _ -> (None, acc)
        in
        Some {Alpha.Pat.find_index; find_name}
    in
    let exp () = Some () in
    let abs ptc l mode p = function
      | Bound (j, i) -> Bound (j, i)
      | Free x ->
        match mode with
        | `Pat -> Free x
        | `Exp ->
          match fst (ptc.Alpha.Pat.find_name p x) with
          | Some i -> Bound (l, i)
          | None -> Free x
    in
    let sub ptc l mode p = function
      | Free x -> Free x
      | Bound (j, k) ->
        match mode with
        | `Pat -> assert false
        | `Exp ->
          if Int.eq j l then
            match fst (ptc.Alpha.Pat.find_index p k) with
            | Some x -> Free x
            | None -> Bound (j, k)
          else
            Bound (j, k)
    in
    let aeq t1 t2 =
      match (t1, t2) with
      | (Bound (i1, j1), Bound (i2, j2)) -> Int.eq i1 i2 && Int.eq j1 j2
      | (Free x1, Free x2) -> Any_name.equal x1 x2
      | _ -> false
    in
    {Alpha.pat; exp; abs; sub; aeq}
end
*)

let equal tc a b = tc.Alpha.aeq a b

*)
