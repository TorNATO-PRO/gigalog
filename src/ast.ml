type program = statement list

and statement =
  | Output of string
  | Clause of clause 

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

let clauses (prog: program): clause list = List.filter_map (
  function
    | Clause c -> Some c
    | _ -> None 
  ) prog

let output_predicates (prog: program): string list = List.filter_map (
  function 
    | Output o -> Some o
    | _ -> None
  ) prog
