open Std_internal

include Interned_string.Make (struct
  let initial_table_size = 100
end)

let type_name = Type.Name.create ~name:"Constant.t"

let () = Swap.register type_name (fun _ c -> c)

let type_rep  = Type.Rep.Abstract type_name

module Map_type_name =
  Type.Name.Make1 (struct
    let name = "Constant.Map"
    type 'a t = 'a Map.t
  end)

let type_name_of_map = Map_type_name.lookup

let type_rep_of_map a =
  let name = type_name_of_map (Type.Rep.id a) in
  Swap.register name (fun p m -> Map.map m ~f:(Swap.swap a p));
  Type.Rep.Abstract name

let pretty t = Pretty.text (to_string t)
