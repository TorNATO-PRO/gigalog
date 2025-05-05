open Ast
open Graph

type edge_kind = PosEdge | NegEdge

(* Graph module definition *)
module DependencyGraph = struct
  module StringLabel = struct
    type t = string

    let compare = Stdlib.compare
    let equal = ( = )
    let hash = Hashtbl.hash
  end

  module EdgeKind = struct
    type t = edge_kind

    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal = ( = )
    let default = PosEdge
  end

  include Imperative.Digraph.ConcreteLabeled (StringLabel) (EdgeKind)
end

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

type stratification_error = NegativeCycle of string list

(* Graph building *)
let build_graph (prog : program) : DependencyGraph.t =
  let g = DependencyGraph.create () in

  let add_vertex p =
    if not (DependencyGraph.mem_vertex g p) then DependencyGraph.add_vertex g p
  in

  List.iter
    (fun clause ->
      match clause with
      | Fact p -> add_vertex p.name
      | Rule (head, body) ->
          add_vertex head.name;
          List.iter
            (fun lit ->
              match lit with
              | Pos p ->
                  add_vertex p.name;
                  DependencyGraph.add_edge_e g
                    (DependencyGraph.E.create p.name PosEdge head.name)
              | Neg p ->
                  add_vertex p.name;
                  DependencyGraph.add_edge_e g
                    (DependencyGraph.E.create p.name NegEdge head.name))
            body)
    (clauses prog);

  g

(* String conversion utilities *)
let string_of_edge_kind = function PosEdge -> "+" | NegEdge -> "-"

let string_of_graph (graph : DependencyGraph.t) : string =
  let buffer = Buffer.create 128 in
  DependencyGraph.iter_vertex
    (fun v ->
      let edges = DependencyGraph.succ_e graph v in
      if edges <> [] then
        let deps =
          List.map
            (fun edge ->
              let dst = DependencyGraph.E.dst edge in
              let label = DependencyGraph.E.label edge in
              Printf.sprintf "%s(%s)" dst (string_of_edge_kind label))
            edges
        in
        Buffer.add_string buffer
          (Printf.sprintf "%s -> %s\n" v (String.concat ", " deps)))
    graph;
  Buffer.contents buffer

(* Find cycles with at least one negative edge *)
let has_negative_cycle (graph : DependencyGraph.t) :
    (string list) option =
  (* First, find strongly connected components *)
  let module SCC = Graph.Components.Make (DependencyGraph) in
  let sccs = SCC.scc_list graph in

  (* For each SCC, check if it contains a negative edge *)
  let rec process_sccs = function
    | [] -> None
    | scc :: rest ->
        (* If SCC has only one node, check for self-loops *)
        if List.length scc = 1 then
          let node = List.hd scc in
          let self_edges = DependencyGraph.find_all_edges graph node node in
          if
            List.exists
              (fun e -> DependencyGraph.E.label e = NegEdge)
              self_edges
          then Some [ node ]
          else process_sccs rest
        else
          (* Create a subgraph for this SCC *)
          let sub_g = DependencyGraph.create () in
          List.iter (fun v -> DependencyGraph.add_vertex sub_g v) scc;
          List.iter
            (fun v ->
              DependencyGraph.iter_succ_e
                (fun e ->
                  let dst = DependencyGraph.E.dst e in
                  if List.mem dst scc then DependencyGraph.add_edge_e sub_g e)
                graph v)
            scc;

          (* Check if there are any negative edges *)
          let has_negative_edge = ref false in
          DependencyGraph.iter_edges_e
            (fun e ->
              if DependencyGraph.E.label e = NegEdge then
                has_negative_edge := true)
            sub_g;

          if !has_negative_edge then
            Some scc
          else process_sccs rest
  in
  process_sccs sccs

(* Check stratification *)
let check_stratification (graph : DependencyGraph.t) :
    (unit, stratification_error) result =
  match has_negative_cycle graph with
  | Some cycle -> Error (NegativeCycle cycle)
  | None -> Ok ()

(* Compute strongly connected components for a given set of predicates *)
let compute_sccs (graph : DependencyGraph.t) (preds : string list) :
    string list list =
  let module SCC = Graph.Components.Make (DependencyGraph) in
  let sub_graph = DependencyGraph.create () in

  (* Add vertices for predicates in the set *)
  List.iter
    (fun p ->
      if DependencyGraph.mem_vertex graph p then
        DependencyGraph.add_vertex sub_graph p)
    preds;

  (* Add edges between these predicates *)
  List.iter
    (fun p ->
      if DependencyGraph.mem_vertex sub_graph p then
        DependencyGraph.iter_succ_e
          (fun edge ->
            let dst = DependencyGraph.E.dst edge in
            if List.mem dst preds then DependencyGraph.add_edge_e sub_graph edge)
          graph p)
    preds;

  SCC.scc_list sub_graph

(* Identify recursive predicates from SCCs *)
let recursive_predicates (graph : DependencyGraph.t) (preds : string list) :
    StringSet.t =
  let sccs = compute_sccs graph preds in

  (* A predicate is recursive if:
     1. It's in an SCC with more than one predicate, or
     2. It has a self-loop *)
  List.filter
    (fun scc ->
      List.length scc > 1
      || List.length scc = 1
         &&
         let p = List.hd scc in
         DependencyGraph.mem_edge graph p p)
    sccs
  |> List.concat |> StringSet.of_list

(* Stratify the program *)
let stratify (graph : DependencyGraph.t) :
    ((string -> int) * (int -> StringSet.t), stratification_error) result =
  match check_stratification graph with
  | Error err -> Error err
  | Ok () ->
      (* Initialize all predicates to stratum 0 *)
      let stratum = ref StringMap.empty in
      DependencyGraph.iter_vertex
        (fun v -> stratum := StringMap.add v 0 !stratum)
        graph;

      (* Iteratively assign strata based on dependencies
         - We need to keep increasing strata until a fixpoint is reached
         - For negative edges, the target must be in a higher stratum
         - For positive edges, the target must be in at least the same stratum *)
      let changed = ref true in
      while !changed do
        changed := false;
        DependencyGraph.iter_vertex
          (fun to_ ->
            DependencyGraph.iter_pred_e
              (fun edge ->
                let from = DependencyGraph.E.src edge in
                let label = DependencyGraph.E.label edge in
                let from_stratum = StringMap.find from !stratum in
                let to_stratum = StringMap.find to_ !stratum in
                let new_stratum =
                  match label with
                  | PosEdge -> max to_stratum from_stratum
                  | NegEdge -> max to_stratum (from_stratum + 1)
                in
                if new_stratum > to_stratum then (
                  stratum := StringMap.add to_ new_stratum !stratum;
                  changed := true))
              graph to_)
          graph
      done;

      (* Group predicates by stratum *)
      let stratum_preds = ref IntMap.empty in
      StringMap.iter
        (fun pred s ->
          let preds =
            Option.value ~default:[] (IntMap.find_opt s !stratum_preds)
          in
          stratum_preds := IntMap.add s (pred :: preds) !stratum_preds)
        !stratum;

      (* Compute recursive predicates for each stratum *)
      let recursive_preds_map =
        IntMap.map
          (fun preds -> recursive_predicates graph preds)
          !stratum_preds
      in

      Ok
        ( (fun pred ->
            StringMap.find_opt pred !stratum |> Option.value ~default:0),
          fun stratum ->
            IntMap.find_opt stratum recursive_preds_map
            |> Option.value ~default:StringSet.empty )
