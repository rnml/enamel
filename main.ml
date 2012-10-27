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
  let ty = Sexp.input_sexp stdin |! Base.Fix.Type.t_of_sexp in
  let (ty, ki) = Base.Fix.Type.ok Initial_context.ctx ty in
  Sexp.List [
    Systemf.Type.sexp_of_t ty;
    Sexp.Atom ":";
    Systemf.Kind.sexp_of_t ki;
  ]
  |! Sexp.to_string_hum
  |! print_endline

let () = Exn.handle_uncaught ~exit:true main
