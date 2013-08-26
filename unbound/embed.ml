open Std_internal

type 'term t = 'term with sexp

let create x = x

module Type_name =
  Type.Name.Make1 (struct
    let name = "Unbound.Embed"
    type nonrec 't t = 't t
  end)

let type_name = Type_name.lookup

let type_rep a =
  let name = type_name (Type.Rep.id a) in
  let rep = a in
  Free_vars.Pat.register name (Free_vars.Term.fold rep);
  Binders.register name (fun names _ -> names);
  Swap.register name (Swap.swap rep);
  Type.Rep.Abstract name

