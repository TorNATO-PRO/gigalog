open Gigaloglib

let parse_lexbuf lexbuf =
    try
        let program  = Parser.program Lexer.read lexbuf in
        program
          |> Eval.eval_program
          |> Eval.pp_db
          |> print_endline
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

(* Entry point *)
let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <input file>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(1) in
  parse_file filename
