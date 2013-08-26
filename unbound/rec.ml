open Std_internal

type 'pat t = 'pat with sexp

let create x = x

module Type_name =
  Type.Name.Make1 (struct
    let name = "Unbound.Rec"
    type nonrec 'p t = 'p t
  end)

let type_name = Type_name.lookup

let type_rep a =
  let name = type_name (Type.Rep.id a) in
  let rep = a in
  Free_vars.Pat.register name (Free_vars.Pat.fold rep);
  Swap.register name (Swap.swap rep);
  Type.Rep.Abstract name
