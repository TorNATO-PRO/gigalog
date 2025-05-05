open Ast
module IntMap = Map.Make (Int)
module PMap = Map.Make (String)
module StringSet = Dependency.StringSet

module TupleSet = Set.Make (struct
  type t = atom list

  let compare = Stdlib.compare
end)

type tuple = atom list
type db = TupleSet.t PMap.t

let empty_db : db = PMap.empty

let insert_fact (pred : string) (tup : tuple) (d : db) : db =
  let bucket = Option.value ~default:TupleSet.empty (PMap.find_opt pred d) in
  PMap.add pred (TupleSet.add tup bucket) d

let insert_facts pred tuples d =
  List.fold_left (fun db t -> insert_fact pred t db) d tuples

let all_facts d =
  PMap.bindings d
  |> List.concat_map (fun (p, ts) ->
         List.map (fun args -> (p, args)) (TupleSet.elements ts))

let pp_db (d : db) =
  d |> PMap.bindings
  |> List.sort (fun (name1, _) (name2, _) -> compare name1 name2)
  |> List.map (fun (name, tuples) ->
         List.map
           (fun args ->
             Printf.sprintf "%s(%s)" name
               (String.concat "," (List.map Ast_printer.string_of_atom args)))
           (TupleSet.elements tuples))
  |> List.flatten |> List.sort String.compare |> String.concat "\n"

(* --- Renaming Utilities --- *)

let fresh_name base id = base ^ "#" ^ string_of_int id

let freshen_atom env next_id = function
  | Var v when v = "_" ->
      let v' = fresh_name v next_id in
      (Var v', (v, v') :: env, next_id + 1)
  | Var v -> (
      match List.assoc_opt v env with
      | Some v' -> (Var v', env, next_id)
      | None ->
          let v' = fresh_name v next_id in
          (Var v', (v, v') :: env, next_id + 1))
  | (Const _ | Str _) as c -> (c, env, next_id)

let freshen_atoms env next_id args =
  let rec loop acc env id = function
    | [] -> (List.rev acc, env, id)
    | a :: as_ ->
        let a', env', id' = freshen_atom env id a in
        loop (a' :: acc) env' id' as_
  in
  loop [] env next_id args

let freshen_predicate env id p =
  let args', env', id' = freshen_atoms env id p.args in
  ({ p with args = args' }, env', id')

let freshen_clause id = function
  | Fact p ->
      let p', _, id' = freshen_predicate [] id p in
      (Fact p', id')
  | Rule (head, body) ->
      let head', env, id1 = freshen_predicate [] id head in
      let body', _, id2 =
        List.fold_right
          (fun lit (acc, env_acc, id_acc) ->
            match lit with
            | Pos p ->
                let p', env', id' = freshen_predicate env_acc id_acc p in
                (Pos p' :: acc, env', id')
            | Neg p ->
                let p', env', id' = freshen_predicate env_acc id_acc p in
                (Neg p' :: acc, env', id'))
          body ([], env, id1)
      in
      (Rule (head', body'), id2)

(* --- Parallel Utilities --- *)

let parallel_filter_map pool f xs =
  let results = Array.make (List.length xs) None in
  Domainslib.Task.parallel_for pool ~start:0
    ~finish:(List.length xs - 1)
    ~body:(fun i ->
      let x = List.nth xs i in
      results.(i) <- f x);
  Array.to_list results |> List.filter_map Fun.id

(* --- Core Evaluation Functions --- *)

let matches pool d pred =
  match PMap.find_opt pred.name d with
  | None -> []
  | Some tuples ->
      let ts = TupleSet.elements tuples in
      let subs =
        parallel_filter_map pool (fun tup -> Subst.unify_opt pred.args tup) ts
      in
      subs

let join (pool : Domainslib.Task.pool) db acc_substs lits =
  List.fold_left
    (fun acc_substs lit ->
      let step theta =
        match lit with
        | Pos atom ->
            let atom' =
              { atom with args = Subst.apply_atoms theta atom.args }
            in
            matches pool db atom'
            |> parallel_filter_map pool (fun theta' ->
                   Subst.merge theta theta' |> Result.to_option)
        | Neg atom ->
            let atom' =
              { atom with args = Subst.apply_atoms theta atom.args }
            in
            if matches pool db atom' = [] then [ theta ] else []
      in
      List.concat_map step acc_substs)
    acc_substs lits

(* --- Small-step Evaluation --- *)

let active_constants (d : db) : atom list =
  d |> PMap.bindings
  |> List.concat_map (fun (_pred, tuples) ->
         List.concat_map
           (List.filter_map (function
             | Const c -> Some (Const c)
             | Str s -> Some (Str s)
             | Var _ -> None))
           (TupleSet.elements tuples))
  |> List.sort_uniq Stdlib.compare

let cartesian_substs vars consts =
  let rec go acc = function
    | [] -> [ acc ]
    | v :: vs ->
        List.concat_map
          (fun theta -> List.map (fun c -> (v, c) :: theta) consts)
          (go acc vs)
  in
  go [] vars

let fire_empty_rule db head =
  let vars =
    head.args
    |> List.fold_left
         (fun seen a ->
           match a with
           | Var v when not (List.mem v seen) -> seen @ [ v ]
           | _ -> seen)
         []
  in
  let consts = active_constants db in
  let substs = cartesian_substs vars consts in
  List.fold_left
    (fun db theta ->
      let grounded_args = Subst.apply_atoms theta head.args in
      insert_fact head.name grounded_args db)
    db substs

let fire_rule pool db clause =
  match clause with
  | Rule (head, body) ->
      let substs = join pool db [ [] ] body in
      let tuples =
        List.map (fun theta -> Subst.apply_atoms theta head.args) substs
      in
      insert_facts head.name tuples db
  | Fact _ -> invalid_arg "fire_rule: expected Rule, got Fact"

let fire_clause pool d = function
  | Fact p -> insert_fact p.name p.args d
  | Rule (head, []) -> fire_empty_rule d head
  | Rule _ as r -> fire_rule pool d r

(* --- List Utilities --- *)

let rec take n lst =
  if n = 0 then []
  else match lst with [] -> [] | x :: xs -> x :: take (n - 1) xs

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | _ :: xs -> drop (n - 1) xs

(* --- Seminaive Evaluation --- *)

let stratum_predicates clauses =
  List.fold_left
    (fun acc clause ->
      match clause with
      | Fact p -> p.name :: acc
      | Rule (head, _) -> head.name :: acc)
    [] clauses
  |> List.sort_uniq String.compare

let compute_new_tuples_for_rule pool db_total delta recursive_preds rule =
  match rule with
  | Rule (head, body) ->
      (* Find positions of recursive predicates in the body *)
      let rec_indices =
        List.mapi
          (fun i lit ->
            match lit with
            | Pos p when StringSet.mem p.name recursive_preds -> Some i
            | _ -> None)
          body
        |> List.filter_map Fun.id
      in

      (* If no recursive predicates, use the traditional evaluation *)
      if rec_indices = [] then
        let substs = join pool db_total [ [] ] body in
        let tuples =
          List.map (fun theta -> Subst.apply_atoms theta head.args) substs
        in
        let existing =
          Option.value ~default:TupleSet.empty
            (PMap.find_opt head.name db_total)
        in
        List.filter (fun tup -> not (TupleSet.mem tup existing)) tuples
      else
        (* For each recursive predicate position, generate new tuples *)
        List.fold_left
          (fun acc i ->
            (* For each recursive predicate position, do a specialized join *)
            let lit_i = List.nth body i in
            let p_name =
              match lit_i with
              | Pos p -> p.name
              | _ -> failwith "Expected positive literal"
            in

            let delta_facts = PMap.find_opt p_name delta in
            match delta_facts with
            (* Skip if no new facts for this predicate *)
            | None -> acc
            | Some x when TupleSet.is_empty x -> acc
            (* Perform semi-naive evaluation when there are new facts*)
            | Some delta_facts ->
                (* Process each subgoal with at least one delta fact:
                1. Join all literals before i using the total database
                2. Join the i-th literal using delta facts only
                3. Join all literals after i using the total database *)
                let substs_before = join pool db_total [ [] ] (take i body) in

                (* Create a temporary database with only delta facts for this predicate *)
                let temp_delta_db = PMap.singleton p_name delta_facts in

                (* Join with the delta for the i-th predicate *)
                let substs_i =
                  join pool temp_delta_db substs_before [ lit_i ]
                in

                (* Join with the remaining literals *)
                let substs_after =
                  join pool db_total substs_i (drop (i + 1) body)
                in

                (* Generate new tuples and filter out existing ones *)
                let tuples =
                  List.map
                    (fun theta -> Subst.apply_atoms theta head.args)
                    substs_after
                in
                let existing =
                  Option.value ~default:TupleSet.empty
                    (PMap.find_opt head.name db_total)
                in

                (* Add new tuples to accumulator, avoiding duplicates *)
                let all = List.fold_left (fun s tup -> TupleSet.add tup s) TupleSet.empty tuples in
                let new_tuples = TupleSet.diff all existing in
                TupleSet.elements new_tuples)
          [] rec_indices
  | Fact _ -> []

let fixpoint_stratum stratum_of recursive_preds_for_stratum pool clauses d =
  let stratum_preds = stratum_predicates clauses in

  (* Initialize with facts *)
  let db_with_facts =
    List.fold_left
      (fun db clause ->
        match clause with
        | Fact _ ->
            let cl', _ = freshen_clause 0 clause in
            fire_clause pool db cl'
        | _ -> db)
      d clauses
  in

  (* Find non-recursive rules (those without recursive predicates in body) *)
  let current_stratum =
    match clauses with
    | [] -> 0
    | Fact p :: _ -> stratum_of p.name
    | Rule (head, _) :: _ -> stratum_of head.name
  in
  let recursive_preds = recursive_preds_for_stratum current_stratum in

  (* Apply non-recursive rules first *)
  let initial_db =
    List.fold_left
      (fun db clause ->
        match clause with
        | Rule (_, body)
          when List.for_all
                 (function
                   | Pos p -> not (StringSet.mem p.name recursive_preds)
                   | Neg _ -> true)
                 body ->
            let cl', _ = freshen_clause 0 clause in
            fire_clause pool db cl'
        | _ -> db)
      db_with_facts clauses
  in

  (* Prepare initial delta - all facts derived so far for this stratum *)
  let initial_delta =
    PMap.mapi
      (fun p ts -> if List.mem p stratum_preds then ts else TupleSet.empty)
      initial_db
  in

  (* Iterative fixpoint computation *)
  let rec iterate db_total delta =
    (* Check if we've reached a fixpoint *)
    if PMap.for_all (fun _ ts -> TupleSet.is_empty ts) delta then db_total
    else
      (* Find all rules with recursive predicates in this stratum *)
      let recursive_rules =
        List.filter
          (fun cl ->
            match cl with
            | Rule (head, body) ->
                List.mem head.name stratum_preds
                && List.exists
                     (function
                       | Pos p -> StringSet.mem p.name recursive_preds
                       | _ -> false)
                     body
            | _ -> false)
          clauses
      in

      (* Compute new delta from recursive rules *)
      let new_delta = PMap.map (fun _ -> TupleSet.empty) delta in
      let new_delta =
        List.fold_left
          (fun acc_delta rule ->
            match rule with
            | Rule (head, _) ->
                let new_tuples =
                  compute_new_tuples_for_rule pool db_total delta
                    recursive_preds rule
                in
                if new_tuples = [] then acc_delta
                else
                  let current =
                    Option.value ~default:TupleSet.empty
                      (PMap.find_opt head.name acc_delta)
                  in
                  PMap.add head.name
                    (TupleSet.union current (TupleSet.of_list new_tuples))
                    acc_delta
            | _ -> acc_delta)
          new_delta recursive_rules
      in

      (* Update the database with new facts *)
      let db_total' =
        PMap.fold
          (fun p ts acc_db ->
            if TupleSet.is_empty ts then acc_db
            else insert_facts p (TupleSet.elements ts) acc_db)
          new_delta db_total
      in

      (* Continue with next iteration *)
      iterate db_total' new_delta
  in

  iterate initial_db initial_delta

let group_by_stratum prog stratum_of =
  List.fold_left
    (fun acc clause ->
      let stratum =
        match clause with Fact _ -> 0 | Rule (head, _) -> stratum_of head.name
      in
      let group = Option.value ~default:[] (IntMap.find_opt stratum acc) in
      IntMap.add stratum (clause :: group) acc)
    IntMap.empty prog

let print_clause_groups groups =
  IntMap.bindings groups
  |> List.iter (fun (stratum, clauses) ->
         Printf.printf "Stratum %d:\n" stratum;
         List.iter
           (fun clause ->
             match clause with
             | Fact p ->
                 Printf.printf "  Fact: %s(%s)\n" p.name
                   (String.concat ","
                      (List.map Ast_printer.string_of_atom p.args))
             | Rule (head, body) ->
                 Printf.printf "  Rule: %s(%s) :- %s\n" head.name
                   (String.concat ","
                      (List.map Ast_printer.string_of_atom head.args))
                   (String.concat ", "
                      (List.map
                         (function
                           | Pos p ->
                               Printf.sprintf "%s(%s)" p.name
                                 (String.concat ","
                                    (List.map Ast_printer.string_of_atom p.args))
                           | Neg p ->
                               Printf.sprintf "not %s(%s)" p.name
                                 (String.concat ","
                                    (List.map Ast_printer.string_of_atom p.args)))
                         body)))
           clauses;
         print_endline "")

let display_edge_label edge_label =
  match edge_label with
  | Dependency.NegEdge -> "-Neg->"
  | Dependency.PosEdge -> "-Pos->"

let display_negative_cycle (cycle : string list) :
    string =
  match cycle with
  | x :: _ ->
      List.map
        (fun vertex -> vertex)
        cycle
      |> String.concat ""
      |> fun a -> a ^ x
  | [] -> ""

let eval_program pool prog =
  let clauses' = clauses prog in
  let graph = Dependency.build_graph prog in
  match Dependency.stratify graph with
  | Error (NegativeCycle cycle) ->
      failwith
        ("Stratification error, negative cycle detected: "
        ^ display_negative_cycle cycle)
  | Ok (stratum_of, recursive_preds_for_stratum) ->
      let clause_groups = group_by_stratum clauses' stratum_of in
      let strata =
        IntMap.bindings clause_groups
        |> List.sort (fun (s1, _) (s2, _) -> compare s1 s2)
      in
      List.fold_left
        (fun db (_, clauses) ->
          fixpoint_stratum stratum_of recursive_preds_for_stratum pool clauses
            db)
        empty_db strata
