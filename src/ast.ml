type program = statement list

and input = { name : string; path : string }

and expr =
  | EConst of int
  | EVar of string
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EPow of expr * expr
  | ENeg of expr

and statement =
  | Directive of directive
  | Clause of clause 

and directive =
  | Input of input
  | Output of string

and clause =
  | Fact of predicate
  | Rule of predicate * lit list

and lit =
  | Pos of predicate
  | Neg of predicate
  | Eq of expr * expr
  | Neq of expr * expr
  | Geq of expr * expr
  | Leq of expr * expr
  | LT of expr * expr
  | GT of expr * expr

and predicate = { name : string; args : atom list }

and atom =
  | Var of string
  | Int of int
  | Sym of string
  | Str of string
  | Expr of expr

let clauses (prog: program): clause list = List.filter_map (
  function
    | Clause c -> Some c
    | _ -> None 
  ) prog

let output_directives (prog: program): string list = List.filter_map (
  function
    | Directive (Output o) -> Some o
    | _ -> None
  ) prog

let input_directives (prog: program): input list  = List.filter_map (
  function 
    | Directive (Input i) -> Some i
    | _ -> None
  ) prog
