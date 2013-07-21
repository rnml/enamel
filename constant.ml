open Std_internal

include Interned_string.Make (struct
  let initial_table_size = 100
end)

let pretty t = Pretty.text (to_string t)

