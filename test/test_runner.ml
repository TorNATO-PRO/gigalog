open Gigaloglib

(* Helpers *)
let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let path_join parts =
  List.fold_left Filename.concat "" parts

let project_root =
  match Sys.getenv_opt "TEST_ROOT" with
  | Some path -> path
  | None -> failwith "TEST_ROOT not set"

let path_in_test name ext =
  path_join [ project_root; "test"; "programs"; name ^ ext ]

(* Colors *)
let green s = "\027[32m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"

(* Pretty diff function *)
let pretty_diff expected got =
  let expected_lines = String.split_on_char '\n' expected in
  let got_lines = String.split_on_char '\n' got in
  let rec walk e g =
    match (e, g) with
    | [], [] -> ()
    | exp :: es, got :: gs when exp = got ->
        walk es gs
    | exp :: es, got :: gs ->
        Printf.printf "%s %s\n" (red "-") exp;
        Printf.printf "%s %s\n" (green "+") got;
        walk es gs
    | exp :: es, [] ->
        Printf.printf "%s %s\n" (red "-") exp;
        walk es []
    | [], got :: gs ->
        Printf.printf "%s %s\n" (green "+") got;
        walk [] gs
  in
  walk expected_lines got_lines

(* Parse file *)
let parse_file name =
  let text = read_file (path_in_test name ".dl") in
  let lexbuf = Lexing.from_string text in
  try
    Parser.program Lexer.read lexbuf
  with
    Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      failwith (Printf.sprintf "Parse error in %s at line %d, col %d"
                  name pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

let run_test name : bool =
  print_endline "";
  let prog = parse_file name in
  let db = Eval.eval_program prog in
  let output = Eval.pp_db db |> String.trim in
  let expected = read_file (path_in_test name ".out") |> String.trim in
  if output = expected then begin
    Printf.printf "%s %s\n" (green "[PASS]") name;
    true
  end else begin
    Printf.printf "%s %s\n" (red "[FAIL]") name;
    pretty_diff expected output;
    false
  end

let tests = [
  "simple_parent";
  "simple_variable";
  "chain_grandparent";
  "quoted_names";
  "no_match_case";
  "self_recursive";
  "ancestor_base";
  "ancestor_recursive";
  "parent_strings";
  "friends_transitive";
  "mutual_connected";
  "not_sibling";
  "neg_success";
  "neg_failure";
  "double_negation";
  "neg_empty";
]

(* Main runner *)
let () =
  let results = List.map run_test tests in
  if List.for_all (fun passed -> passed) results then
    exit 0
  else
    exit 1
