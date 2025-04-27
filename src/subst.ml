open Ast

type subst = (string * atom) list

let apply_atom theta = function
  | Var v ->
      (* Find var v in the association list and replace it *)
      (match List.assoc_opt v theta with
       | Some t -> t
       | None   -> Var v)
  | x -> x

let apply_atoms theta = List.map (apply_atom theta)

type unify_error = [ `Clash of string | `Const_mismatch of string * string | `Arity_mismatch ]

let ok x = Ok x
let error e = Error e
let ( let* ) r f = match r with
 | Ok x -> f x
 | Error _ as e -> e 

let extend v t theta : subst = (v, t) :: theta

let merge theta1 theta2 : (subst, unify_error) result =
    let rec go acc = function
    | [] -> ok acc
    | (v, t) :: rest ->
         match List.assoc_opt v acc with
        | None -> go ((v, t) :: acc) rest
        | Some t' when t = t' -> go acc rest
        | Some _ -> error (`Clash v)
    in
    go theta1 theta2

let unify_atoms theta a1 a2 : (subst, unify_error) result =
    let a1 = apply_atom theta a1
    and a2 = apply_atom theta a2 in
    match a1, a2 with
    | Var v, t | t, Var v -> ok (extend v t theta)
    | Const c1, Const c2 | Str c1, Str c2 when c1 = c2 -> ok theta
    | Const c1, Const c2 -> error (`Const_mismatch (c1,c2))
    | Str  s1 , Str  s2  -> error (`Const_mismatch (s1,s2))
    | _                  -> error (`Clash "incompatible atoms")

let rec unify_lists theta l1 l2 : (subst, unify_error) result =
    match l1, l2 with
    | [], [] -> ok theta
    | a1::r1, a2::r2 ->
        let* theta1  = unify_atoms theta a1 a2 in
        unify_lists theta1 (apply_atoms theta1 r1) (apply_atoms theta1 r2)
    | _ -> error `Arity_mismatch

let unify_atoms_opt a b =
    match unify_atoms [] a b with
    | Ok theta        -> Some theta
    | Error _     -> None

let unify_opt l1 l2 =
    match unify_lists [] l1 l2 with
    | Ok theta    -> Some theta
    | Error _ -> None
      