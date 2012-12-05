open Std_internal

include String

let store : t String.Table.t = String.Table.create ~size:100 ()

let of_string x = Table.find_or_add store x ~default:(fun () -> x)
let to_string x = x

let t_of_sexp s = of_string (t_of_sexp s)

let pretty = Pretty.text

