open Std_internal

include Interned_string.Make (struct
  let initial_table_size = 100
end)

let pretty t = Pretty.text (to_string t)

let type_name = Type.Name.create ~name:"Constant.t"
let type_rep  = Type.Rep.Abstract type_name

