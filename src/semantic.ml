open Ast

module StringMap = Map.Make(String)

let predicate_arity p = (p.name, List.length p.args)

let lit_predicate_arity = function
  | Pos p -> predicate_arity p
  | Neg p -> predicate_arity p

let clause_predicate_arities = function
  | Fact pred -> [predicate_arity pred]
  | Rule (pred, list) -> (predicate_arity pred) :: List.map lit_predicate_arity list

let statement_predicate_arities = function
  | Clause c -> clause_predicate_arities c
  | _ -> []

let arityErrorMessage pred expectedArity actualArity =
  "Expected " ^ string_of_int expectedArity ^ " arguments for " ^ pred ^ ", got: " ^ string_of_int actualArity
    
let arities_match (prog: program) : string list =
  let (_, errors) =
    List.fold_left
      (fun (arityMap, errors) statement ->
        List.fold_left
          (fun (arityMap, errors) (pred, arity) ->
            match StringMap.find_opt pred arityMap with
            | Some expectedArity ->
                if arity = expectedArity then
                  (arityMap, errors)
                else
                  (arityMap, arityErrorMessage pred expectedArity arity :: errors)
            | None ->
                (StringMap.add pred arity arityMap, errors)
          )
          (arityMap, errors)
          (statement_predicate_arities statement)
      )
      (StringMap.empty, [])
      prog
  in
  errors

let errors prog = arities_match prog
