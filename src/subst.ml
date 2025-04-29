open Ast
open Expr

type subst = (string * atom) list

let apply_expr theta expr =
  let rec aux = function
    | EConst n -> EConst n
    | EVar v -> (
        match List.assoc_opt v theta with
        | Some (Expr e') -> aux e'
        | _ -> EVar v)
    | ENeg e -> ENeg (aux e)
    | EAdd (l, r) -> EAdd (aux l, aux r)
    | ESub (l, r) -> ESub (aux l, aux r)
    | EMul (l, r) -> EMul (aux l, aux r)
    | EDiv (l, r) -> EDiv (aux l, aux r)
    | EPow (l, r) -> EPow (aux l, aux r)
  in
  aux expr

let apply_atom theta = function
  | Var v -> (
      match List.assoc_opt v theta with
      | Some t -> t
      | None -> Var v)
  | Int _ as i -> i
  | Sym _ as s -> s
  | Str _ as s -> s
  | Expr e ->
      let e' = apply_expr theta e in
      match eval_expr theta e' with
      | Some n -> Int n
      | None -> Expr e'

let apply_atoms theta = List.map (apply_atom theta)

type unify_error =
  [ `Clash of string
  | `Int_mismatch of int * int
  | `Sym_mismatch of string * string
  | `Arity_mismatch ]

let ok x = Ok x
let error e = Error e
let ( let* ) = Result.bind

let extend v t theta = (v, t) :: theta

let merge theta1 theta2 =
  let rec aux acc = function
    | [] -> Ok acc
    | (v, t) :: rest -> (
        match List.assoc_opt v acc with
        | None -> aux ((v, t) :: acc) rest
        | Some t' when t = t' -> aux acc rest
        | Some _ -> Error (`Clash v))
  in
  aux theta1 theta2

let unify_atom theta a1 a2 =
  let a1 = apply_atom theta a1 in
  let a2 = apply_atom theta a2 in
  match a1, a2 with
  | Var v, t | t, Var v -> Ok (extend v t theta)
  | Int n1, Int n2 when n1 = n2 -> Ok theta
  | Int n1, Int n2 -> Error (`Int_mismatch (n1, n2))
  | Sym s1, Sym s2 when s1 = s2 -> Ok theta
  | Sym s1, Sym s2 -> Error (`Sym_mismatch (s1, s2))
  | Str s1, Str s2 when s1 = s2 -> Ok theta
  | Str s1, Str s2 -> Error (`Sym_mismatch (s1, s2))
  | Expr e1, Expr e2 -> (
      match eval_expr theta e1, eval_expr theta e2 with
      | Some n1, Some n2 when n1 = n2 -> Ok theta
      | Some n1, Some n2 -> Error (`Int_mismatch (n1, n2))
      | _ -> Error (`Clash "expr"))
  | Expr e, Int n | Int n, Expr e -> (
      match eval_expr theta e with
      | Some n' when n' = n -> Ok theta
      | Some n' -> Error (`Int_mismatch (n', n))
      | None -> Error (`Clash "expr"))
  | _ -> Error (`Clash "incompatible atoms")

let rec unify_lists theta l1 l2 =
  match l1, l2 with
  | [], [] -> Ok theta
  | a1 :: r1, a2 :: r2 ->
      let* theta1 = unify_atom theta a1 a2 in
      unify_lists theta1 (apply_atoms theta1 r1) (apply_atoms theta1 r2)
  | _ -> Error `Arity_mismatch

let unify_opt l1 l2 =
  match unify_lists [] l1 l2 with
  | Ok theta -> Some theta
  | Error _ -> None

let unify_atom_opt a b =
  match unify_atom [] a b with
  | Ok theta -> Some theta
  | Error _ -> None
