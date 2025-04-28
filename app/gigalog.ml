open Gigaloglib
open Ast

let write_buffer_to_file filename content =
  let oc = open_out filename in
  Buffer.output_buffer oc content;
  close_out oc

let parse_lexbuf lexbuf =
    try
        let program = Parser.program Lexer.read lexbuf in
        program
    with
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "Syntax error at line %d, column %d\n"
            pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
        exit 1

let parse_factfile_lexbuf factfile factname lexbuf =
  try
    let facts = Facts_parser.facts Facts_lexer.read lexbuf in
    List.map (fun fact -> Ast.Fact Ast.{ name = factname; args = fact }) facts
  with
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "Syntax error in %s at line %d, column %d\n"
        factfile pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    exit 1

let parse_fact_file filename factname =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  parse_factfile_lexbuf filename factname lexbuf

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  parse_lexbuf lexbuf

  let extend_program_with_facts (prog: Ast.program) : Ast.program =
    let input_predicates = Ast.input_predicates prog in
    let new_facts =
      List.concat_map (fun input ->
        parse_fact_file input.path input.name
        |> List.map (fun clause -> Ast.Clause clause)
      ) input_predicates
    in
    new_facts @ prog



(* (* let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s *)

let read_facts factsLocation =
  let file_input = read_file factsLocation in
  let newline_re = Str.regexp "\r?\n" in
  let comma_re = Str.regexp "[ \t]*,[ \t]*" in
  file_input
  |> Str.split newline_re
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map (Str.split comma_re)

let process_input program =
  Ast.input_predicates program
  |> List.concat_map (fun pred -> 
      let facts = read_facts pred.path in
      List.map (fun fact -> Ast.Fact { name = pred.name; args = fact }) facts
      
    )

 *)
(* let extend_program_with_facts program facts =
  facts :: program *)

let output_db_to_file pred set =
  let buffer = Buffer.create 1024 in

  List.iter (fun elems ->
    List.iter (fun atom ->
      Buffer.add_string buffer (Ast_printer.string_of_atom atom);
      Buffer.add_string buffer " "
    ) elems;
    Buffer.add_string buffer "\n"
  ) (Eval.TupleSet.elements set);

  write_buffer_to_file (pred ^ ".txt") buffer 
  
      
let execute_program program =
  let db = Eval.eval_program program in
  let output_preds = Ast.output_predicates program in

  List.iter (fun pred ->
    match Eval.PMap.find_opt pred db with
    | Some set -> output_db_to_file pred set
    | None -> ()
  ) output_preds

(* Entry point *)
let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <input file>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let program = parse_file filename in
  let extended_program = extend_program_with_facts program in
  let semanticErrors = Semantic.errors extended_program in
  if List.is_empty semanticErrors then begin
    execute_program extended_program;
    print_endline "Processed program successfully"
  end else begin
    print_endline "Errors found, please correct them";
    List.iter (fun err ->
      print_string "> ";
      print_endline err
    ) semanticErrors
  end
