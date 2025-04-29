open Ast
open Dependency

module StringMap = Map.Make(String)

let analyze (prog : program) : unit =
    let graph = build_graph prog in
    print_endline "\nDependency Graph:";
    print_endline (string_of_graph graph);
    let cycles = find_cycles graph in
    print_endline "\nDetected Cycles:";
    List.iter (fun cycle ->
      Printf.printf "Cycle: %s\n" (String.concat " -> " cycle)  
    ) cycles

let predicate_arity p = (p.name, List.length p.args)

let lit_predicate_arity = function
  | Pos p -> Some (predicate_arity p)
  | Neg p -> Some (predicate_arity p)
  | _ -> None

let clause_predicate_arities = function
  | Fact pred -> [predicate_arity pred]
  | Rule (pred, list) -> (predicate_arity pred) :: List.filter_map lit_predicate_arity list

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
