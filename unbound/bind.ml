open Std_internal

type ('pat, 'term) t = 'pat * 'term with sexp

let create p t = (p, t)

let unbind typ tyt (p, t) =
  let fvs = Free_vars.Term.fv tyt t in
  let (p, perm) = Freshen.fresh_wrt typ p ~fvs in
  (p, Swap.swap tyt perm t)

let fvs_term a b acc (p, t) =
  Set.union acc begin
    let p = Free_vars.Pat.fold  a Name.Univ.Set.empty p in
    let t = Free_vars.Term.fold b Name.Univ.Set.empty t in
    Set.diff t p
  end

module Type_name =
  Type.Name.Make2 (struct
    let name = "Unbound.Bind"
    type nonrec ('p, 't) t = ('p, 't) t
  end)

let type_name = Type_name.lookup

let type_rep a b =
  let name = type_name (Type.Rep.id a) (Type.Rep.id b) in
  Free_vars.Term.register name (fvs_term a b);
  let rep = Type.Rep.Pair (a, b) in
  Swap.register name (Swap.swap rep);
  Type.Rep.Abstract name

