%{
open Ast
%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token COMMA
%token EOF

%start facts
%type <(atom list) list> facts

%%

facts:
  | fact_list EOF { $1 }

fact_list:
  | { [] }
  | fact fact_list { $1 :: $2 }

fact:
  | atoms { $1 }

atoms:
  | atom { [$1] }
  | atom COMMA atoms { $1 :: $3 }

atom:
  | id = IDENT {
      if id = "_" then
        Var "_"
      else if Char.uppercase_ascii id.[0] = id.[0] then
        Var id
      else
        Sym id
    }
  | s = STRING { Str s }
  | i = INT { Int i }
