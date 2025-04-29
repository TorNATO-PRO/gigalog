open Ast

module IntMap = Map.Make (Int)
module PMap = Map.Make (String)

module TupleSet = Set.Make(struct
  type t = atom list
  let compare = Stdlib.compare
end)

type tuple  = atom list
type db     = TupleSet.t PMap.t

let empty_db : db = PMap.empty

let insert_fact (pred : string) (tup : atom list) (d : db) : db =
  let bucket = Option.value ~default:TupleSet.empty (PMap.find_opt pred d) in
  PMap.add pred (TupleSet.add tup bucket) d

let insert_facts pred tuples d =
  List.fold_left (fun db t -> insert_fact pred t db) d tuples

let all_facts d =
  PMap.bindings d
  |> List.concat_map (fun (p,ts) ->
         List.map (fun args -> (p,args)) ts)
        
let pp_db (d: db) =
    d
    |> PMap.bindings
    |> List.sort (fun (name1, _) (name2, _) -> compare name1 name2)
    |> List.map (fun (name, tuples) ->
         List.map (fun args ->
           Printf.sprintf "%s(%s)" name
             (String.concat "," (List.map Ast_printer.string_of_atom args))
         ) (TupleSet.elements tuples)
       )
    |> List.flatten
    |> List.sort String.compare
    |> String.concat "\n"
  

(* --- Renaming Utilities --- *)

let fresh_name (base : string) (id : int) : string =
  base ^ "#" ^ string_of_int id

let rec freshen_expr env id expr =
  match expr with
  | EConst _ -> (expr, env, id)
  | EVar v ->
      begin match List.assoc_opt v env with
      | Some v' -> (EVar v', env, id)
      | None ->
          let v' = fresh_name v id in
          (EVar v', (v, v') :: env, id + 1)
      end
  | EAdd (e1, e2) ->
      let (e1', env1, id1) = freshen_expr env id e1 in
      let (e2', env2, id2) = freshen_expr env1 id1 e2 in
      (EAdd (e1', e2'), env2, id2)
  | ESub (e1, e2) ->
      let (e1', env1, id1) = freshen_expr env id e1 in
      let (e2', env2, id2) = freshen_expr env1 id1 e2 in
      (ESub (e1', e2'), env2, id2)
  | EMul (e1, e2) ->
      let (e1', env1, id1) = freshen_expr env id e1 in
      let (e2', env2, id2) = freshen_expr env1 id1 e2 in
      (EMul (e1', e2'), env2, id2)
  | EDiv (e1, e2) ->
      let (e1', env1, id1) = freshen_expr env id e1 in
      let (e2', env2, id2) = freshen_expr env1 id1 e2 in
      (EDiv (e1', e2'), env2, id2)
  | EPow (e1, e2) ->
      let (e1', env1, id1) = freshen_expr env id e1 in
      let (e2', env2, id2) = freshen_expr env1 id1 e2 in
      (EPow (e1', e2'), env2, id2)
  | ENeg e ->
      let (e', env', id') = freshen_expr env id e in
      (ENeg e', env', id')

let int_nth_root (c : int) (n : int) : int option =
  if n < 0 then None else
  let rec try_root r =
    let rec pow a k =
      if k = 0 then 1
      else a * pow a (k - 1)
    in
    let p = pow r n in
    if p = c then Some r
    else if p > c then None
    else try_root (r + 1)
  in
  try_root 0

let int_log (c : int) (b : int) : int option =
  if b <= 1 || c < 1 then None else
  let rec aux k acc =
    if acc = c then Some k
    else if acc > c then None
    else aux (k + 1) (acc * b)
  in
  aux 0 1

let freshen_atom env next_id = function
  | Var v ->
    if v = "_" then
      (* Handle wildcards *)
      let v' = fresh_name v next_id in
      let env = (v, v') :: env in
      (Var v', env, next_id + 1)
    else
      begin match List.assoc_opt v env with
      | Some v' -> (Var v', env, next_id)
      | None ->
        let v' = fresh_name v next_id in
        let env = (v, v') :: env in
        (Var v', env, next_id + 1)
      end
  | (Sym _ | Str _) as c -> (c, env, next_id)
  | a -> (a, env, next_id)

let freshen_atoms env next_id args =
  let rec loop acc env id = function
    | [] -> (List.rev acc, env, id)
    | a :: as_ ->
      let (a', env', id') = freshen_atom env id a in
      loop (a' :: acc) env' id' as_
  in
  loop [] env next_id args

let freshen_predicate env id (p : predicate) = 
  let (args', env', id') = freshen_atoms env id p.args in
  ({ p with args = args' }, env', id')

let freshen_clause id = function
  | Fact p ->
    let (p', _, id') = freshen_predicate [] id p in
    (Fact p', id')
  | Rule (head, body) ->
    let (head', env, id1) = freshen_predicate [] id head in
    let (body', _, id2) =
      List.fold_right
        (fun lit (acc, env_acc, id_acc) ->
          match lit with
          | Pos p ->
              let (p', env', id') = freshen_predicate env_acc id_acc p in
              (Pos p' :: acc, env', id')
          | Neg p ->
              let (p', env', id') = freshen_predicate env_acc id_acc p in
              (Neg p' :: acc, env', id')
          | Eq (e1, e2) ->
            let (e1', env1, id1) = freshen_expr env_acc id_acc e1 in
            let (e2', env2, id2) = freshen_expr env1 id1 e2 in
            (Eq (e1', e2') :: acc, env2, id2)
          | Neq (e1, e2) ->
            let (e1', env1, id1) = freshen_expr env_acc id_acc e1 in
            let (e2', env2, id2) = freshen_expr env1 id1 e2 in
            (Neq (e1', e2') :: acc, env2, id2)
          | Geq (e1, e2) ->
            let (e1', env1, id1) = freshen_expr env_acc id_acc e1 in
            let (e2', env2, id2) = freshen_expr env1 id1 e2 in
            (Geq (e1', e2') :: acc, env2, id2)
          | Leq (e1, e2) ->
            let (e1', env1, id1) = freshen_expr env_acc id_acc e1 in
            let (e2', env2, id2) = freshen_expr env1 id1 e2 in
            (Leq (e1', e2') :: acc, env2, id2)
          | LT (e1, e2) ->
            let (e1', env1, id1) = freshen_expr env_acc id_acc e1 in
            let (e2', env2, id2) = freshen_expr env1 id1 e2 in
            (LT (e1', e2') :: acc, env2, id2)
          | GT (e1, e2) ->
            let (e1', env1, id1) = freshen_expr env_acc id_acc e1 in
            let (e2', env2, id2) = freshen_expr env1 id1 e2 in
            (GT (e1', e2') :: acc, env2, id2))
        body
        ([], env, id1)
    in
    (Rule (head', body'), id2)


let parallel_filter_map pool f xs =
  let results = Array.make (List.length xs) None in
  Domainslib.Task.parallel_for pool ~start:0 ~finish:(List.length xs - 1) ~body:(fun i ->
    let x = List.nth xs i in
    results.(i) <- f x
  );
  Array.to_list results |> List.filter_map Fun.id

(* --- The heart of our datalog engine --- *)

let matches (pool: Domainslib.Task.pool) (d : db) (pred : predicate) : Subst.subst list =
  match PMap.find_opt pred.name d with
  | None -> []
  | Some tuples ->
      TupleSet.elements tuples
      |> parallel_filter_map pool (fun tup ->
            Subst.unify_opt pred.args tup)

let join (pool: Domainslib.Task.pool) (db: db) (acc_substs: Subst.subst list) (lits: lit list) =
  List.fold_left
    (fun acc_substs lit ->
        let step theta =
          match lit with
          | Pos atom ->
              let atom' = { atom with args = Subst.apply_atoms theta atom.args } in
              matches pool db atom'
              |> parallel_filter_map pool (fun theta' ->
                  Subst.merge theta theta' |> Result.to_option)

          | Neg atom ->
              let atom' = { atom with args = Subst.apply_atoms theta atom.args } in
              if matches pool db atom' = [] then [theta] else []

          | Eq  (e1, e2) ->
              (match Expr.eval_expr theta e1, Expr.eval_expr theta e2 with
              | Some v1, Some v2 when v1 = v2 -> [theta]
              | _ -> [])

          | Neq (e1, e2) ->
              (match Expr.eval_expr theta e1, Expr.eval_expr theta e2 with
              | Some v1, Some v2 when v1 <> v2 -> [theta]
              | _ -> [])

          | Leq (e1, e2) ->
              (match Expr.eval_expr theta e1, Expr.eval_expr theta e2 with
              | Some v1, Some v2 when v1 <= v2 -> [theta]
              | _ -> [])

          | Geq (e1, e2) ->
              (match Expr.eval_expr theta e1, Expr.eval_expr theta e2 with
              | Some v1, Some v2 when v1 >= v2 -> [theta]
              | _ -> [])

          | LT  (e1, e2) ->
              (match Expr.eval_expr theta e1, Expr.eval_expr theta e2 with
              | Some v1, Some v2 when v1 <  v2 -> [theta]
              | _ -> [])

          | GT  (e1, e2) ->
              (match Expr.eval_expr theta e1, Expr.eval_expr theta e2 with
              | Some v1, Some v2 when v1 >  v2 -> [theta]
              | _ -> [])
        in
        List.concat_map step acc_substs
    )
    acc_substs
    lits

let active_constants (d : db) : atom list =
  d
  |> PMap.bindings
  |> List.concat_map (fun (_pred, tuples) ->
        TupleSet.elements tuples
        |> List.concat_map (fun tuple ->
             List.filter_map (function
               | Sym c -> Some (Sym c)
               | Str  s -> Some (Str s)
               | Int _ -> None
               | Var _ -> None
               | Expr _ -> None) tuple))
  |> List.sort_uniq Stdlib.compare

let cartesian_substs vars consts =
  let rec go acc = function
    | []   -> [acc]
    | v::vs ->
      List.concat_map (fun theta ->
        List.map (fun c -> (v,c)::theta) consts
      ) (go acc vs)
  in
  go [] vars

let fire_empty_rule (db : db) (head : predicate) : db =
  let vars =
    head.args
    |> List.fold_left (fun seen a ->
          match a with
          | Var v when not (List.mem v seen) -> seen @ [v]
          | _ -> seen
        ) []
  in
  let consts = active_constants db in
  let substs = cartesian_substs vars consts in
  List.fold_left (fun db theta ->
    let grounded_args = Subst.apply_atoms theta head.args in
    insert_fact head.name grounded_args db
  ) db substs

let fire_rule (pool: Domainslib.Task.pool) db clause : db =
  match clause with
   | Rule (head, body) -> 
      let substs = join pool db [ [] ] body in
      let tuples =
        List.map (fun theta -> Subst.apply_atoms theta head.args) substs
      in
      insert_facts head.name tuples db
  | Fact _ ->
  invalid_arg "fire_rule: expected Rule, got Fact"

let fire_clause (pool: Domainslib.Task.pool) db = function
  | Fact head
    when List.exists (function Var _ -> true | _ -> false) head.args ->
      fire_empty_rule db head
  | Fact head -> insert_fact head.name head.args db
  | Rule (head, []) -> fire_empty_rule db head
  | Rule _ as r -> fire_rule pool db r


(* --- Until we hit a fixpoint -- **)

let group_by_stratum (prog : clause list) (stratum_of : string -> int) : clause list IntMap.t =
  List.fold_left (fun acc clause ->
    let stratum =
      match clause with
      | Fact _ -> 0
      | Rule (head, _) -> stratum_of head.name
    in
    let group = Option.value ~default:[] (IntMap.find_opt stratum acc) in
    IntMap.add stratum (clause :: group) acc
  ) IntMap.empty prog

let print_clause_groups (groups : clause list IntMap.t) : unit =
  IntMap.bindings groups
  |> List.iter (fun (stratum, clauses) ->
      Printf.printf "Stratum %d:\n" stratum;
      List.iter (fun clause ->
        match clause with
        | Fact p ->
            Printf.printf "  Fact: %s(%s)\n"
              p.name
              (String.concat "," (List.map Ast_printer.string_of_atom p.args))
        | Rule (head, body) ->
            Printf.printf "  Rule: %s(%s) :- %s\n"
              head.name
              (String.concat "," (List.map Ast_printer.string_of_atom head.args))
              (String.concat ", "
                  (List.map (function
                    | Pos p ->
                        Printf.sprintf "%s(%s)"
                          p.name
                          (String.concat "," (List.map Ast_printer.string_of_atom p.args))
                    | Neg p ->
                        Printf.sprintf "not %s(%s)"
                          p.name
                          (String.concat "," (List.map Ast_printer.string_of_atom p.args))
                    | Eq (left, right) -> Printf.sprintf "%s = %s" (Ast_printer.string_of_expr left) (Ast_printer.string_of_expr right)
                    | Neq (left, right) -> Printf.sprintf "%s /= %s" (Ast_printer.string_of_expr left) (Ast_printer.string_of_expr right)
                    | Geq (left, right) -> Printf.sprintf "%s >= %s" (Ast_printer.string_of_expr left) (Ast_printer.string_of_expr right)
                    | Leq (left, right) -> Printf.sprintf "%s <= %s" (Ast_printer.string_of_expr left) (Ast_printer.string_of_expr right)
                    | LT (left, right) -> Printf.sprintf "%s < %s" (Ast_printer.string_of_expr left) (Ast_printer.string_of_expr right)
                    | GT (left, right) -> Printf.sprintf "%s > %s" (Ast_printer.string_of_expr left) (Ast_printer.string_of_expr right)
                  ) body))
      ) clauses;
      print_endline ""
    )

let rec fixpoint_stratum (pool: Domainslib.Task.pool) (clauses : clause list) (d : db) : db =
  let (d', _) =
    List.fold_left (fun (db, id) clause ->
      let (cl', id') = freshen_clause id clause in
      (fire_clause pool db cl', id')
    ) (d, 0) clauses
  in
  if d' = d then
    d
  else
    fixpoint_stratum pool clauses d'

let eval_program (pool: Domainslib.Task.pool) (prog : program) : db =
  let clauses' = clauses prog in
  let graph = Dependency.build_graph prog in
  match Dependency.check_stratification graph with
  | Error msg -> failwith ("Stratification error: " ^ msg)
  | Ok () ->
      let stratum_of = Dependency.stratify graph in
      let clause_groups = group_by_stratum clauses' stratum_of in
      let strata = IntMap.bindings clause_groups |> List.sort (fun (s1, _) (s2, _) -> compare s1 s2) in
      List.fold_left (fun db (_stratum, clauses) ->
        fixpoint_stratum pool clauses db
      ) empty_db strata
