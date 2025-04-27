%{
open Ast
%}

%token <string> IDENT
%token <string> STRING
%token LPAREN RPAREN COMMA COLON_DASH DOT NOT
%token EOF

%start program
%type <program> program

%%

program: 
  | clauses EOF { $1 }

clauses:
  | { [] }
  | clause clauses { $1 :: $2 } 

clause: 
  | fact { $1 }
  | rule { $1 }

fact:
  | predicate DOT {
      if List.exists (function Var _ -> true | _ -> false) $1.args then
        Rule ($1, [])
      else
        Fact $1
    }

rule:
  | predicate COLON_DASH body DOT { Rule ($1, $3) }

predicate:
  | IDENT LPAREN args RPAREN { { name = $1; args = $3 } }

args:
  | atom { [$1] }
  | atom COMMA args { $1 :: $3 }

atom:
  | id = IDENT {
     if Char.uppercase_ascii id.[0] = id.[0] then
       Var id
     else
       Const id
   }
  | STRING { Str $1 }

body:
  | lit { [$1] }
  | lit COMMA body { $1 :: $3 }

lit:
  | predicate { Pos $1 }
  | NOT predicate { Neg $2 }
