{
open Parser
}

rule read = parse
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "not" { NOT }
  | "~" { NOT }
  | "output" { OUTPUT }
  | [' ' '\t' '\r'] { read lexbuf }
  | '%' [^ '\n']* { read lexbuf }
  | "/*" { comment lexbuf }
  | ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { IDENT id }
  | '"' ([^'"']* as str) '"' { STRING str }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ":-" { COLON_DASH }
  | "." { DOT }
  | eof { EOF }

and comment = parse
  | "*/" { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
