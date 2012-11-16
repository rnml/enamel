open Std_internal

include struct
  open Name
  open Base
  open Constant
  open Initial_context
  open Uid
  open Target
  open Module_system
end

module F = Systemf
module Enamel = Module_system.Source (Base)

let check_expr_command =
  Command.basic ~summary:"type check a expr (from stdin)"
    Command.Spec.(empty +> const ())
    (fun () ->
      let tm = Sexp.input_sexp stdin |! Enamel.Expr.t_of_sexp in
      let (tm, ty) = Enamel.Expr.ok Initial_context.ctx tm in
      Sexp.List [
        F.Expr.sexp_of_t tm;
        Sexp.Atom ":";
        F.Type.sexp_of_t ty;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let check_type_command =
  Command.basic ~summary:"kind check a type (from stdin)"
    Command.Spec.(empty +> const ())
    (fun () ->
      let ty = Sexp.input_sexp stdin |! Enamel.Type.t_of_sexp in
      let (ty, ki) = Enamel.Type.ok Initial_context.ctx ty in
      Sexp.List [
        F.Type.sexp_of_t ty;
        Sexp.Atom ":";
        F.Kind.sexp_of_t ki;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let elaborate_command =
  Command.basic ~summary:"elaborate a module (from stdin)"
    Command.Spec.(empty +> const ())
    (fun () ->
      let m = Sexp.input_sexp stdin |! Enamel.Mod.t_of_sexp in
      let (t, e) = Enamel.Mod.ok Initial_context.ctx m in
      let t = Target.Asig.to_f t in
      Sexp.List [
        F.Expr.sexp_of_t e;
        Sexp.Atom ":";
        F.Type.sexp_of_t t;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let init_ctx_command =
  Command.basic ~summary:"show initial typing context"
    Command.Spec.(empty +> const ())
    (fun () ->
      Target.Context.sexp_of_t Initial_context.ctx
      |! Sexp.to_string_hum
      |! print_endline
    )


let command =
  Command.group ~summary:"enamel: my little language" [
    ("check-expr",   check_expr_command);
    ("check-type",   check_type_command);
    ("elaborate",    elaborate_command);
    ("initial-ctx",  init_ctx_command);
  ]

let main () = Command.run command

let () = Exn.handle_uncaught ~exit:true main

