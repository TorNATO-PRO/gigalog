open Ast

let rec string_of_expr = function
  | EConst n -> string_of_int n
  | EVar v -> v
  | ENeg e -> "-" ^ string_paren 4 e
  | EAdd (l, r) ->
      let sl = string_paren 1 l in
      let sr = string_paren 1 r in
      sl ^ " + " ^ sr
  | ESub (l, r) ->
      let sl = string_paren 1 l in
      let sr = string_paren 2 r in
      sl ^ " - " ^ sr
  | EMul (l, r) ->
      let sl = string_paren 3 l in
      let sr = string_paren 3 r in
      sl ^ " * " ^ sr
  | EDiv (l, r) ->
      let sl = string_paren 3 l in
      let sr = string_paren 3 r in
      sl ^ " / " ^ sr
  | EPow (l, r) ->
      let sl = string_paren 4 l in
      let sr = string_paren 4 r in
      sl ^ " ^ " ^ sr

and string_paren parent_prec expr =
  let (prec, s) = match expr with
    | EConst _ | EVar _ -> (5, string_of_expr expr)
    | ENeg _ -> (4, string_of_expr expr)
    | EPow _ -> (3, string_of_expr expr)
    | EMul _ | EDiv _ -> (2, string_of_expr expr)
    | EAdd _ | ESub _ -> (1, string_of_expr expr)
  in
  if prec < parent_prec then
    "(" ^ s ^ ")"
  else s

let string_of_atom = function
  | Var v -> v
  | Int n -> string_of_int n
  | Sym s -> s
  | Str s -> "\"" ^ s ^ "\""
  | Expr e -> string_of_expr e

let string_of_predicate p =
  Printf.sprintf "%s(%s)"
    p.name
    (String.concat ", " (List.map string_of_atom p.args))

let string_of_literal = function
  | Pos p -> string_of_predicate p
  | Neg p -> "~" ^ string_of_predicate p
  | Eq (expl, expr) -> (string_of_expr expl) ^ "=" ^  (string_of_expr expr) 
  | Neq (expl, expr) -> (string_of_expr expl) ^ "/=" ^  (string_of_expr expr) 
  | Geq (expl, expr) -> (string_of_expr expl) ^ ">=" ^  (string_of_expr expr) 
  | Leq (expl, expr) -> (string_of_expr expl) ^ "<=" ^  (string_of_expr expr) 
  | LT (expl, expr) -> (string_of_expr expl) ^ "<" ^  (string_of_expr expr) 
  | GT (expl, expr) -> (string_of_expr expl) ^ ">" ^  (string_of_expr expr) 


let string_of_clause = function
  | Fact p ->
      Printf.sprintf "%s." (string_of_predicate p)
  | Rule (head, body) ->
      let body_str = String.concat ", " (List.map string_of_literal body) in
      Printf.sprintf "%s :- %s."
        (string_of_predicate head)
        body_str

let string_of_input (i: input)  =
  Printf.sprintf ".input %s(%s)." i.name i.path

let string_of_output o =
  Printf.sprintf ".output %s." o

let string_of_statement = function
  | Clause c -> string_of_clause c
  | Directive (Input i) -> string_of_input i
  | Directive (Output i) -> string_of_output i

let string_of_program prog =
  String.concat "\n" (List.map string_of_statement prog)
