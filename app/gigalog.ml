open Gigaloglib

let write_buffer_to_file filename content =
  let oc = open_out filename in
  Buffer.output_buffer oc content;
  close_out oc

let parse_lexbuf lexbuf =
    try
        let program  = Parser.program Lexer.read lexbuf in
        program
    with
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "Syntax error at line %d, column %d\n"
            pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
        exit 1

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  parse_lexbuf lexbuf
        

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
  let semanticErrors = Semantic.errors program in
  if List.is_empty semanticErrors then begin
    execute_program program;
    print_endline "Processed program successfully"
  end else begin
    print_endline "Errors found, please correct them";
    List.iter (fun err ->
      print_string "> ";
      print_endline err
    ) semanticErrors
  end
