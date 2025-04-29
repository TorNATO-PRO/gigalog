{
open Parser
}

rule read = parse
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | [' ' '\t' '\r'] { read lexbuf }
  | "%" { comment lexbuf }
  | "/*" { comment lexbuf }
  | ['0'-'9']+ as num { INT (int_of_string num) }
  | "~" { NOT }
  | "not" { NOT }
  | "input" { INPUT }
  | "output" { OUTPUT }
  | ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { IDENT id }
  | '"' ([^'"']* as str) '"' { STRING str }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | "/=" { NEQ }
  | '/' { DIV }
  | '^' { POW }
  | '=' { EQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "<=" { LEQ }
  | '<' { LESSTHAN }
  | ">=" { GEQ }
  | '>' { GREATERTHAN }
  | ',' { COMMA }
  | ":-" { COLON_DASH }
  | "." { DOT }
  | eof { EOF }
  | _ as c {
    let msg =
      Printf.sprintf "Unknown token: '%c' at position %d"
        c
        (Lexing.lexeme_start lexbuf)
    in
    failwith msg
  }

and comment = parse
  | "*/" { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | eof { EOF }
  | _ { comment lexbuf }
