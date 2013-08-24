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

module Enamel = Module_system.Source (Base)

let check_expr_command =
  Command.basic ~summary:"type check a expr (from stdin)"
    Command.Spec.(empty)
    (fun () ->
      let tm = Sexp.input_sexp stdin |! Enamel.Tm.t_of_sexp in
      let (tm, ty) = Enamel.Tm.ok Initial_context.ctx tm in
      Sexp.List [
        F.Tm.sexp_of_t tm;
        Sexp.Atom ":";
        F.Ty.sexp_of_t ty;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let check_type_command =
  Command.basic ~summary:"kind check a type (from stdin)"
    Command.Spec.(empty)
    (fun () ->
      let ty = Sexp.input_sexp stdin |! Enamel.Ty.t_of_sexp in
      let (ty, ki) = Enamel.Ty.ok Initial_context.ctx ty in
      Sexp.List [
        F.Ty.sexp_of_t ty;
        Sexp.Atom ":";
        F.Kind.sexp_of_t ki;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let elaborate_command =
  Command.basic ~summary:"elaborate a module (from stdin)"
    Command.Spec.(empty)
    (fun () ->
      let m = Sexp.input_sexp stdin |! Enamel.Mod.t_of_sexp in
      let (t, e) = Enamel.Mod.ok Initial_context.ctx m in
      let t = Target.Asig.to_f t in
      Sexp.List [
        F.Tm.sexp_of_t e;
        Sexp.Atom ":";
        F.Ty.sexp_of_t t;
      ]
      |! Sexp.to_string_hum
      |! print_endline
    )

let init_ctx_command =
  Command.basic ~summary:"show initial typing context"
    Command.Spec.(empty)
    (fun () ->
      Target.Context.sexp_of_t Initial_context.ctx
      |! Sexp.to_string_hum
      |! print_endline
    )

module Check_f_type = struct
  let command =
    Command.basic ~summary:"validate a F type from stdin"
      Command.Spec.(empty)
      (fun () ->
        In_channel.input_all stdin
        |! (fun x -> print_endline "1"; x)
        |! String.strip
        |! (fun x -> print_endline "2"; x)
        |! Sexp.of_string
        |! (fun x -> print_endline "3"; x)
        |! F.Ty.t_of_sexp
        |! (fun x -> print_endline "4"; x)
        |! F.Ty.ok F.Ty.Context.empty
        |! (fun x -> print_endline "5"; x)
        |! Or_error.ok_exn
        |! (fun x -> print_endline "6"; x)
        |! F.Kind.sexp_of_t
        |! (fun x -> print_endline "7"; x)
        |! Sexp.to_string_hum
        |! (fun x -> print_endline "8"; x)
        |! print_endline
      )
end

module Check_f_term = struct
  let command =
    Command.basic ~summary:"validate a F term from stdin"
      Command.Spec.(empty)
      (fun () ->
        In_channel.input_all stdin
        |! String.strip
        |! Sexp.of_string
        |! F.Tm.t_of_sexp
        |! F.Tm.ok F.Tm.Context.empty
        |! Or_error.ok_exn
        |! F.Ty.sexp_of_t
        |! Sexp.to_string_hum
        |! print_endline
      )
end

module Z = struct

  let command =
    Command.basic ~summary:"scratch work command"
      Command.Spec.(empty)
      (fun () ->
        In_channel.input_all stdin
        |! String.strip
        |! Sexp.of_string
        |! F.Tm.t_of_sexp
        |! F.Tm.ok F.Tm.Context.empty
        |! Or_error.ok_exn
        |! F.Ty.sexp_of_t
        |! Sexp.to_string_hum
        |! print_endline
      )
end

let command =
  Command.group ~summary:"enamel: my little language" [
    ("check-expr",   check_expr_command);
    ("check-type",   check_type_command);
    ("check-f-type", Check_f_type.command);
    ("check-f-term", Check_f_term.command);
    ("elaborate",    elaborate_command);
    ("initial-ctx",  init_ctx_command);
    ("z",            Z.command);
  ]

let () = print_endline "WHO"

let main () = Command.run command

let () = Exn.handle_uncaught ~exit:true main
