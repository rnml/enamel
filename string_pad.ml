open Core.Std

type t = string list

let empty = []

let rec add t x =
  match t with
  | [] -> [x]
  | y :: ys ->
    if String.length x < String.length y
    then x :: t
    else add ys (y ^ x)

let concat t t' = List.fold ~f:add ~init:t t'

let rec dump_aux acc = function
  | [] -> acc
  | x :: t -> dump_aux (x ^ acc) t

let rec dump = function
  | [] -> ""
  | x :: t -> dump_aux x t
