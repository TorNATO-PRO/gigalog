open Ast

let string_of_atom = function
  | Var id -> id
  | Const c -> c
  | Str s -> "\"" ^ s ^ "\""

let string_of_predicate p =
  Printf.sprintf "%s(%s)"
    p.name
    (String.concat ", " (List.map string_of_atom p.args))

let string_of_literal = function
  | Pos pos -> string_of_predicate pos
  | Neg neg -> "~" ^ string_of_predicate neg

let string_of_clause = function
  | Fact p ->
    Printf.sprintf "Fact: %s." (string_of_predicate p)
  | Rule (head, body) ->
    let body_str = String.concat ", " (List.map string_of_literal body) in
    Printf.sprintf "Rule: %s :- %s." (string_of_predicate head) body_str

let string_of_program (prog : program) =
  String.concat "\n" (List.map string_of_clause prog)
