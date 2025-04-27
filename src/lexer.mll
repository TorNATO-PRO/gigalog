{
open Parser
}

rule read = parse
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "not" { NOT }
  | "~" { NOT }
  | [' ' '\t' '\r'] { read lexbuf }
  | ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { IDENT id }
  | '"' ([^'"']* as str) '"' { STRING str }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ":-" { COLON_DASH }
  | "." { DOT }
  | eof { EOF }
