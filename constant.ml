open Std_internal

include String

let store : t String.Table.t = String.Table.create ~size:100 ()

let of_string x = Table.find_or_add store x ~default:(fun () -> x)

let pretty = Pretty.text

