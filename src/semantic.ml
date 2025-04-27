open Ast
open Dependency

let analyze (prog : program) : unit =
    let graph = build_graph prog in
    print_endline "\nDependency Graph:";
    print_endline (string_of_graph graph);
    let cycles = find_cycles graph in
    print_endline "\nDetected Cycles:";
    List.iter (fun cycle ->
      Printf.printf "Cycle: %s\n" (String.concat " -> " cycle)  
    ) cycles
