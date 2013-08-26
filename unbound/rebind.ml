open Std_internal

type ('pat1, 'pat2) t = 'pat1 * 'pat2 with sexp

let create p1 p2 = (p1, p2)

module Type_name =
  Type.Name.Make2 (struct
    let name = "Unbound.Rebind"
    type nonrec ('p1, 'p2) t = ('p1, 'p2) t
  end)

let type_name = Type_name.lookup

let type_rep a b =
  let name = type_name (Type.Rep.id a) (Type.Rep.id b) in
  let rep = Type.Rep.Pair (a, b) in
  Free_vars.Pat.register name (Free_vars.Pat.fold rep);
  Swap.register name (Swap.swap rep);
  Type.Rep.Abstract name

