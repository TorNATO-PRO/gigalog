%{
open Ast
%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token PLUS MINUS TIMES DIV POW
%token LPAREN RPAREN COMMA COLON_DASH DOT
%token EQ LEQ GREATERTHAN GEQ LESSTHAN NEQ
%token NOT INPUT OUTPUT
%token EOF

%start program
%type <program> program

%left PLUS MINUS
%left TIMES DIV
%right POW
%nonassoc UMINUS

%%

program:
  | statements EOF { $1 }

statements:
  | { [] }
  | clause statements { (Clause $1) :: $2 }
  | directive statements { (Directive $1) :: $2 }

clause:
  | fact DOT { Fact $1 }
  | predicate COLON_DASH body DOT { Rule ($1,$3) }

directive:
  | input { Input $1 }
  | output { Output $1 }

input:
  | DOT INPUT IDENT LPAREN STRING RPAREN { { name = $3; path = $5 }}

output:
  | DOT OUTPUT IDENT { $3 }

fact:
  | predicate { $1 }

predicate:
  | IDENT LPAREN args RPAREN { { name = $1; args = $3 } }
  | IDENT { { name = $1; args = [] } }
args:
  | arg { [$1] }
  | arg COMMA args { $1 :: $3 }

arg:
  | expr { Expr $1 }
  | IDENT {
      if Char.uppercase_ascii $1.[0] = $1.[0]
      then Var $1
      else Sym $1
    }
  | STRING { Str $1 }

body:
  | literal { [$1] }
  | literal COMMA body { $1 :: $3 }

literal:
  | predicate { Pos $1 }
  | NOT predicate { Neg $2 }
  | expr EQ expr { Eq ($1,$3) }
  | expr NEQ expr { Neq ($1,$3) }
  | expr GEQ expr { Geq ($1,$3) }
  | expr LEQ expr { Leq ($1,$3) }
  | expr LESSTHAN expr { LT ($1,$3) }
  | expr GREATERTHAN expr { GT ($1,$3) }

expr:
  | INT { EConst $1 }
  | IDENT { EVar $1 }
  | expr PLUS expr { EAdd ($1,$3) }
  | expr MINUS expr { ESub ($1,$3) }
  | expr TIMES expr { EMul ($1,$3) }
  | expr DIV expr { EDiv ($1,$3) }
  | expr POW expr { EPow ($1,$3) }
  | MINUS expr %prec UMINUS { ENeg $2 }
  | LPAREN expr RPAREN { $2 }
