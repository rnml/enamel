open Std_internal

module Bind = struct
  type ('pat, 'term) t = 'pat * 'term with sexp

  let create p t = (p, t)
  let unbind _ = failwith "Bind.unbind unimplemented"

  let fvs_term a b acc (p, t) =
    Set.union acc begin
      let p = Free_vars.Pat.fv_aux  a New_name.Univ.Set.empty p in
      let t = Free_vars.Term.fv_aux b New_name.Univ.Set.empty t in
      Set.diff t p
    end

  module Type_name = Type.Name.Make2 (struct type nonrec ('p, 't) t = ('p, 't) t end)
  let type_name = Type_name.lookup
  let type_rep a b =
    let name = type_name (Type.Rep.id a) (Type.Rep.id b) in
    Free_vars.Term.register name (fvs_term a b);
    let rep = Type.Rep.Pair (a, b) in
    Swap.register name (Swap.swap rep);
    Type.Rep.Abstract name
end

module Rebind = struct
  type ('pat1, 'pat2) t = 'pat1 * 'pat2 with sexp
  let create p1 p2 = (p1, p2)
  module Type_name = Type.Name.Make2 (struct type nonrec ('p1, 'p2) t = ('p1, 'p2) t end)
  let type_name = Type_name.lookup
  let type_rep a b =
    let name = type_name (Type.Rep.id a) (Type.Rep.id b) in
    let rep = Type.Rep.Pair (a, b) in
    Free_vars.Pat.register name (Free_vars.Pat.fv_aux rep);
    Swap.register name (Swap.swap rep);
    Type.Rep.Abstract name
end

module Embed = struct
  type 'term t = 'term with sexp
  let create x = x
  module Type_name = Type.Name.Make1 (struct type nonrec 't t = 't t end)
  let type_name = Type_name.lookup
  let type_rep a =
    let name = type_name (Type.Rep.id a) in
    let rep = a in
    Free_vars.Pat.register name (Free_vars.Term.fv_aux rep);
    Swap.register name (Swap.swap rep);
    Type.Rep.Abstract name
end

module Rec = struct
  type 'pat t = 'pat with sexp
  let create x = x
  module Type_name = Type.Name.Make1 (struct type nonrec 'p t = 'p t end)
  let type_name = Type_name.lookup
  let type_rep a =
    let name = type_name (Type.Rep.id a) in
    let rep = a in
    Free_vars.Pat.register name (Free_vars.Pat.fv_aux rep);
    Swap.register name (Swap.swap rep);
    Type.Rep.Abstract name
end

