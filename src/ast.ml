type program = clause list

and clause =
  | Fact of predicate
  | Rule of predicate * lit list

and lit =
  | Pos of predicate
  | Neg of predicate

and predicate = { name : string; args : atom list }

and atom =
  | Var of string
  | Const of string
  | Str of string
