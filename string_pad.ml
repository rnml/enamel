open Core.Std

type t =
| Nil
| Snoc of t * string

let to_list t =
  let rec loop t acc =
    match t with
    | Nil -> acc
    | Snoc (t, x) -> loop t (x :: acc)
  in
  loop t []

let empty = Nil

let rec add t x =
  match t with
  | Nil -> Snoc (t, x)
  | Snoc (ys, y) ->
    if String.length x < String.length y
    then Snoc (t, x)
    else add ys (y ^ x)

let concat t1 t2 = List.fold ~f:add ~init:t1 (to_list t2)

let rec dump_aux acc = function
  | Nil -> acc
  | Snoc (t, x) -> dump_aux (x ^ acc) t

let rec dump = function
  | Nil -> ""
  | Snoc (t, x) -> dump_aux x t
