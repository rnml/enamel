open Std_internal

include Interned_string.Make (struct
  let initial_table_size = 100
end)

let type_name = Type.Name.create ~name:"Constant.t"

let () = Swap.register type_name (fun _ c -> c)
let () = Free_vars.Term.register type_name (fun fvs _ -> fvs)
let () = Free_vars.Pat.register type_name (fun fvs _ -> fvs)

let type_rep  = Type.Rep.Abstract type_name

module Map_type_name =
  Type.Name.Make1 (struct
    let name = "Constant.Map"
    type 'a t = 'a Map.t
  end)

let type_name_of_map = Map_type_name.lookup

let fvs fold map acc =
  Map.fold map ~init:acc ~f:(fun ~key:_ ~data acc -> fold acc data)

let type_rep_of_map a =
  let name = type_name_of_map (Type.Rep.id a) in
  Swap.register name (fun p m -> Map.map m ~f:(Swap.swap a p));
  Free_vars.Term.register name (fun acc map -> fvs (Free_vars.Term.fold a) map acc);
  Free_vars.Pat.register name (fun acc map -> fvs (Free_vars.Pat.fold a) map acc);
  Type.Rep.Abstract name

let pretty t = Pretty.text (to_string t)
