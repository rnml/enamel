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

let check_term_command =
  Command.basic ~summary:"type check a term (from stdin)"
    Command.Spec.(empty +> const ())
    (fun () ->
      let tm = Sexp.input_sexp stdin |! Base.Fix.Term.t_of_sexp in
      let (tm, ty) = Base.Fix.Term.ok Initial_context.ctx tm in
      Sexp.List [
        Systemf.Term.sexp_of_t tm;
        Sexp.Atom ":";
        Systemf.Type.sexp_of_t ty;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let check_type_command =
  Command.basic ~summary:"kind check a type (from stdin)"
    Command.Spec.(empty +> const ())
    (fun () ->
      let ty = Sexp.input_sexp stdin |! Base.Fix.Type.t_of_sexp in
      let (ty, ki) = Base.Fix.Type.ok Initial_context.ctx ty in
      Sexp.List [
        Systemf.Type.sexp_of_t ty;
        Sexp.Atom ":";
        Systemf.Kind.sexp_of_t ki;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let command =
  Command.group ~summary:"enamel: my little language" [
    ("check-term", check_term_command);
    ("check-type", check_type_command);
  ]

let main () = Command.run command

let () = Exn.handle_uncaught ~exit:true main

