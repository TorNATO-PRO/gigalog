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

let insert_fact (pred : string) (tup : tuple) (d : db) : db =
  let bucket = Option.value ~default:TupleSet.empty (PMap.find_opt pred d) in
  PMap.add pred (TupleSet.add tup bucket) d

let insert_facts pred tuples d =
  List.fold_left (fun db t -> insert_fact pred t db) d tuples

let all_facts d =
  PMap.bindings d
  |> List.concat_map (fun (p,ts) ->
         List.map (fun args -> (p,args)) (TupleSet.elements ts))

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

let fresh_name base id = base ^ "#" ^ string_of_int id

let freshen_atom env next_id = function
  | Var v when v = "_" ->
      let v' = fresh_name v next_id in
      (Var v', (v, v') :: env, next_id + 1)
  | Var v ->
      begin match List.assoc_opt v env with
      | Some v' -> (Var v', env, next_id)
      | None ->
          let v' = fresh_name v next_id in
          (Var v', (v, v') :: env, next_id + 1)
      end
  | (Const _ | Str _) as c -> (c, env, next_id)

let freshen_atoms env next_id args =
  let rec loop acc env id = function
    | [] -> (List.rev acc, env, id)
    | a :: as_ ->
        let (a', env', id') = freshen_atom env id a in
        loop (a' :: acc) env' id' as_
  in
  loop [] env next_id args

let freshen_predicate env id p =
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
                (Neg p' :: acc, env', id'))
          body
          ([], env, id1)
      in
      (Rule (head', body'), id2)

(* --- Parallel Utilities --- *)

let parallel_filter_map pool f xs =
  let results = Array.make (List.length xs) None in
  Domainslib.Task.parallel_for pool ~start:0 ~finish:(List.length xs - 1) ~body:(fun i ->
    let x = List.nth xs i in
    results.(i) <- f x
  );
  Array.to_list results |> List.filter_map Fun.id

(* --- Core Evaluation Functions --- *)

let matches (pool: Domainslib.Task.pool) (d : db) (pred : predicate) : Subst.subst list =
  match PMap.find_opt pred.name d with
  | None -> []
  | Some tuples ->
      TupleSet.elements tuples
      |> parallel_filter_map pool (fun tup -> Subst.unify_opt pred.args tup)

let join (pool: Domainslib.Task.pool) db acc_substs lits =
  List.fold_left (fun acc_substs lit ->
    let step theta =
      match lit with
      | Pos atom ->
          let atom' = { atom with args = Subst.apply_atoms theta atom.args } in
          matches pool db atom'
          |> parallel_filter_map pool (fun theta' -> Subst.merge theta theta' |> Result.to_option)
      | Neg atom ->
          let atom' = { atom with args = Subst.apply_atoms theta atom.args } in
          if matches pool db atom' = [] then [theta] else []
    in
    List.concat_map step acc_substs
  ) acc_substs lits

(* --- Small-step Evaluation --- *)

let active_constants (d : db) : atom list =
  d
  |> PMap.bindings
  |> List.concat_map (fun (_pred, tuples) ->
       List.concat_map
         (List.filter_map (function Const c -> Some (Const c) | Str s -> Some (Str s) | Var _ -> None))
         (TupleSet.elements tuples))
  |> List.sort_uniq Stdlib.compare

let cartesian_substs vars consts =
  let rec go acc = function
    | [] -> [acc]
    | v::vs ->
        List.concat_map (fun theta -> List.map (fun c -> (v,c)::theta) consts) (go acc vs)
  in
  go [] vars

let fire_empty_rule db head =
  let vars =
    head.args
    |> List.fold_left (fun seen a -> match a with Var v when not (List.mem v seen) -> seen @ [v] | _ -> seen) []
  in
  let consts = active_constants db in
  let substs = cartesian_substs vars consts in
  List.fold_left (fun db theta ->
    let grounded_args = Subst.apply_atoms theta head.args in
    insert_fact head.name grounded_args db
  ) db substs

let fire_rule pool db clause =
  match clause with
  | Rule (head, body) ->
      let substs = join pool db [ [] ] body in
      let tuples = List.map (fun theta -> Subst.apply_atoms theta head.args) substs in
      insert_facts head.name tuples db
  | Fact _ -> invalid_arg "fire_rule: expected Rule, got Fact"

let fire_clause pool d = function
  | Fact p -> insert_fact p.name p.args d
  | Rule (head, []) -> fire_empty_rule d head
  | Rule _ as r -> fire_rule pool d r

(* --- List Utilities --- *)

let rec take n lst =
  if n = 0 then [] else match lst with [] -> [] | x :: xs -> x :: take (n - 1) xs

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | _ :: xs -> drop (n - 1) xs

(* --- Seminaive Evaluation --- *)

let stratum_predicates clauses =
  List.fold_left (fun acc clause ->
    match clause with Fact p -> p.name :: acc | Rule (head, _) -> head.name :: acc
  ) [] clauses
  |> List.sort_uniq String.compare

let compute_new_tuples_for_rule pool db_total delta stratum_preds rule =
  match rule with
  | Rule (head, body) ->
      let rec_indices =
        List.mapi (fun i lit -> match lit with Pos p when List.mem p.name stratum_preds -> Some i | _ -> None) body
        |> List.filter_map Fun.id
      in
      let existing = Option.value ~default:TupleSet.empty (PMap.find_opt head.name db_total) in
      let new_tuples_set =
        List.fold_left (fun acc i ->
          let substs_before = join pool db_total [ [] ] (take i body) in
          let substs_i = List.concat_map (fun theta ->
            let lit_i = List.nth body i in
            let atom_i' = match lit_i with Pos p -> { p with args = Subst.apply_atoms theta p.args } | _ -> failwith "expected Pos" in
            let matches_i = matches pool delta atom_i' in
            List.filter_map (fun theta' -> Subst.merge theta theta' |> Result.to_option) matches_i
          ) substs_before in
          let substs_after = join pool db_total substs_i (drop (i + 1) body) in
          let tuples = List.map (fun theta -> Subst.apply_atoms theta head.args) substs_after in
          List.fold_left (fun acc tup ->
            if not (TupleSet.mem tup existing) then TupleSet.add tup acc else acc
          ) acc tuples
        ) TupleSet.empty rec_indices
      in
      TupleSet.elements new_tuples_set
  | Fact _ -> []

let fixpoint_stratum pool clauses d =
  let stratum_preds = stratum_predicates clauses in
  (* Step 1: Apply all facts first *)
  let facts = List.filter (function Fact _ -> true | _ -> false) clauses in
  let db_with_facts = List.fold_left (fun db clause ->
    let (cl', _) = freshen_clause 0 clause in
    fire_clause pool db cl'
  ) d facts in
  (* Step 2: Apply non-recursive rules and rules with empty bodies using db_with_facts *)
  let initial_clauses =
    List.filter (fun cl -> match cl with
      | Fact _ -> false (* Exclude facts as they are already applied *)
      | Rule (_, body) -> body = [] || List.for_all (function Pos p -> not (List.mem p.name stratum_preds) | Neg _ -> true) body
    ) clauses
  in
  let db_total = List.fold_left (fun db clause ->
    let (cl', _) = freshen_clause 0 clause in
    fire_clause pool db cl'
  ) db_with_facts initial_clauses in
  let initial_delta = PMap.filter (fun p _ -> List.mem p stratum_preds) db_total in
  let rec loop db_total delta =
    if PMap.for_all (fun _ ts -> TupleSet.is_empty ts) delta then db_total else
    let new_delta =
      List.fold_left (fun new_delta p ->
        let rules_for_p =
          List.filter (fun cl -> match cl with Rule (head, body) -> head.name = p && List.exists (function Pos q -> List.mem q.name stratum_preds | _ -> false) body | _ -> false) clauses
        in
        let new_tuples_p =
          List.fold_left (fun acc rule -> acc @ compute_new_tuples_for_rule pool db_total delta stratum_preds rule) [] rules_for_p
          |> List.sort_uniq Stdlib.compare
        in
        let existing = Option.value ~default:TupleSet.empty (PMap.find_opt p db_total) in
        let new_tuples_p = List.filter (fun tup -> not (TupleSet.mem tup existing)) new_tuples_p in
        PMap.add p (TupleSet.of_list new_tuples_p) new_delta
      ) (PMap.map (fun _ -> TupleSet.empty) delta) stratum_preds
    in
    let db_total' = PMap.fold (fun p ts db -> insert_facts p (TupleSet.elements ts) db) new_delta db_total in
    loop db_total' new_delta
  in
  loop db_total initial_delta

let group_by_stratum prog stratum_of =
  List.fold_left (fun acc clause ->
    let stratum = match clause with Fact _ -> 0 | Rule (head, _) -> stratum_of head.name in
    let group = Option.value ~default:[] (IntMap.find_opt stratum acc) in
    IntMap.add stratum (clause :: group) acc
  ) IntMap.empty prog

let print_clause_groups groups =
  IntMap.bindings groups
  |> List.iter (fun (stratum, clauses) ->
        Printf.printf "Stratum %d:\n" stratum;
        List.iter (fun clause ->
          match clause with
          | Fact p -> Printf.printf "  Fact: %s(%s)\n" p.name (String.concat "," (List.map Ast_printer.string_of_atom p.args))
          | Rule (head, body) ->
              Printf.printf "  Rule: %s(%s) :- %s\n"
                head.name
                (String.concat "," (List.map Ast_printer.string_of_atom head.args))
                (String.concat ", " (List.map (function
                  | Pos p -> Printf.sprintf "%s(%s)" p.name (String.concat "," (List.map Ast_printer.string_of_atom p.args))
                  | Neg p -> Printf.sprintf "not %s(%s)" p.name (String.concat "," (List.map Ast_printer.string_of_atom p.args))
                ) body))
        ) clauses;
        print_endline ""
      )

let eval_program pool prog =
  let clauses' = clauses prog in
  let graph = Dependency.build_graph prog in
  match Dependency.stratify graph with
  | Error (NegativeCycle _) -> failwith "Stratification error: Negative Cycle Detected"
  | Ok stratum_of ->
      let clause_groups = group_by_stratum clauses' stratum_of in
      let strata = IntMap.bindings clause_groups |> List.sort (fun (s1, _) (s2, _) -> compare s1 s2) in
      List.fold_left (fun db (_stratum, clauses) -> fixpoint_stratum pool clauses db) empty_db strata
