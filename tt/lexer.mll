{

open Std_internal
open Parser

let con_or_kw = function
  | "Type" -> Kw_type
  | c -> Con (Constant.of_string c)

let var_or_kw = function
  | v -> Var (Term.Name.raw v)

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let idch = lower | upper | digit | [ '\'' '-' ]
let con = upper idch*
let var = ( lower | '_' ) idch*
let space = [ ' ' '\t' ]
let eol = "\r" | "\n" | "\n\r" | "\r\n"

let nested_comment_start = "{-"
let nested_comment_finish = "-}"

rule token = parse
  | ',' { Comma }
  | ':' { Colon }
  | ';' { Semi }
  | '.' { Period }
  | '\\' { Backslash }
  | '(' { Lparen } | ')' { Rparen }
  | "->" { Arrow }
  | nested_comment_start { comment 1 lexbuf }
  | con as c { con_or_kw c }
  | var as v { var_or_kw v }
  | space { token lexbuf }
  | eol { Lexing.new_line lexbuf; token lexbuf }
  | _ as ch { failwithf "unrecognized character '%c'" ch () }
  | eof { EOF }

and comment n = parse
  | eol { Lexing.new_line lexbuf; comment n lexbuf }
  | nested_comment_start
      { comment (n + 1) lexbuf }
  | nested_comment_finish
      { if n > 1 then comment (n - 1) lexbuf else token lexbuf }
  | _
      { comment n lexbuf }
  | eof
      { failwithf "unterminated comment (nesting depth %d)" n () }

{

}
