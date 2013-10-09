open Std_internal

let parse_command =
  Command.basic ~summary:"parser"
    Command.Spec.(
      empty
      +> anon ("SRC-FILE" %: file)
    )
    (fun file () ->
       In_channel.with_file file ~f:(fun cin ->
         let lexbuf = Lexing.from_channel cin in
         Parser.tele_top Lexer.token lexbuf
         |> <:sexp_of<Term.t Term.s>>
         |> Sexp.to_string_hum
         |> print_endline
       )
    )

type token = Parser.token =
  | Arrow
  | Comma
  | Period
  | Backslash
  | Colon
  | Kw_type
  | Lparen
  | Rparen
  | Semi
  | EOF
  | Var of (Term.Name.t)
  | Con of (Constant.t)
with sexp_of

let lex_command =
  Command.basic ~summary:"lexer"
    Command.Spec.(
      empty
      +> anon ("SRC-FILE" %: file)
    )
    (fun file () ->
       In_channel.with_file file ~f:(fun cin ->
         let lexbuf = Lexing.from_channel cin in
         let rec loop () =
           let token = Lexer.token lexbuf in
           print_endline (Sexp.to_string_hum (<:sexp_of<token>> token));
           match token with
           | EOF -> ()
           | _ -> loop ()
         in
         loop ()
       )
    )

let command =
  Command.group ~summary:"type theory" [
    ("parse", parse_command);
    ("lex", lex_command);
  ]

let main () = Command.run command

let () = Exn.handle_uncaught ~exit:true main