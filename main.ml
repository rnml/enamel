open Std_internal

include struct
  open Name
  open Base
  open Constant
  open Initial_context
  open Uid
  open Target
  open Module_system
  open Unbound
end

module F = Systemf
module Enamel = Module_system.Source (Base)

let check_expr_command =
  Command.basic ~summary:"type check a expr (from stdin)"
    Command.Spec.(empty)
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
    Command.Spec.(empty)
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
    Command.Spec.(empty)
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
    Command.Spec.(empty)
    (fun () ->
      Target.Context.sexp_of_t Initial_context.ctx
      |! Sexp.to_string_hum
      |! print_endline
    )

let unbound_gen_command =
  Command.basic ~summary:"generate unbound typing context"
    Command.Spec.(empty +> anon ("SPEC" %: file))
    (fun path () ->
      Sexp.load_sexp_conv_exn path Unbound.Compile_time.Env.t_of_sexp
      |! Unbound.Compile_time.Env.type_defs
      |! Text_block.render
      |! print_endline
    )

module Z = struct

  (* example from http://www.columbia.edu/~cs2035/courses/csor4231.F11/scc.pdf *)
  let a = [
    (1, 2); (2, 3); (3, 4);
    (6, 5); (5, 1); (1, 6);
    (2, 6);
    (6, 7);
    (7, 3); (3, 7);
    (8, 7);
  ]

  (* example from http://www.cs.berkeley.edu/~vazirani/s99cs170/notes/lec12.pdf *)
  let b = [
    (1, 2); (2, 3);
    (2, 4);
    (2, 5); (5, 2);
    (3, 6); (6, 3);
    (4, 5); (5, 6);
    (4, 7);
    (5, 7);
    (6, 8);
    (7, 8); (8, 7);
    (9, 7); (7, 10); (10, 9);
    (10, 11); (11, 12); (12, 10);
  ]

  let command =
    Command.basic ~summary:"scratch work command"
      Command.Spec.(empty)
      (fun () ->
        let module Scc = Scc.Make (Int) in
        Scc.scc a
        |! <:sexp_of< int list list>>
        |! Sexp.to_string_hum
        |! print_endline
      )
end

let command =
  Command.group ~summary:"enamel: my little language" [
    ("check-expr",   check_expr_command);
    ("check-type",   check_type_command);
    ("elaborate",    elaborate_command);
    ("initial-ctx",  init_ctx_command);
    ("unbound-gen",  unbound_gen_command);
    ("z",            Z.command);
  ]

let main () = Command.run command

let () = Exn.handle_uncaught ~exit:true main
