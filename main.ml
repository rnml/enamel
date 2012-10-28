open Std_internal

include struct
  open Name
  open Base
  open Constant
  open Initial_context
  open Uid
  open Systemf
  open Target
  open Module_system
end

let main () =
  let tm = Sexp.input_sexp stdin |! Base.Fix.Term.t_of_sexp in
  let (tm, ty) = Base.Fix.Term.ok Initial_context.ctx tm in
  Sexp.List [
    Systemf.Term.sexp_of_t tm;
    Sexp.Atom ":";
    Systemf.Type.sexp_of_t ty;
  ]
  |! Sexp.to_string_hum
  |! print_endline

let () = Exn.handle_uncaught ~exit:true main
