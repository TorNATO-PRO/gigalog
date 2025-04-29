{
open Facts_parser
}

rule read = parse
  | [' ' '\t' '\r']   { read lexbuf }
  | '\n'              { read lexbuf }
  | ['0'-'9']+ as num { INT (int_of_string num)}
  | ','               { COMMA }
  | '"' ([^ '"'])* '"' as s {
      let len = String.length s in
      let content = String.sub s 1 (len - 2) in
      STRING content
    }
  | ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']* as id { IDENT id }
  | eof               { EOF }
  | _ as c            { failwith (Printf.sprintf "Unexpected character: %c" c) }
