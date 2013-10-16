open Std_internal

type t = Diff_logic.t

let create () = Diff_logic.var ()

let assert_leq t1 t2 = Diff_logic.set_leq t1 (`Plus (t2, 0))
let assert_lt  t1 t2 = Diff_logic.set_lt  t1 (`Plus (t2, 0))

let type_name = Type.Name.create ~name:"Level.t"
let type_rep  = Type.Rep.Abstract type_name

let () = Free_vars.Term.register type_name (fun fvs _ -> fvs)
let () = Free_vars.Pat.register type_name (fun fvs _ -> fvs)

let sexp_of_t = sexp_of_opaque

let () = Swap.register type_name (fun _ x -> x)
