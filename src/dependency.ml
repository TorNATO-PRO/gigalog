open Ast
open Graph

type edge_kind = PosEdge | NegEdge

module DependencyGraph = struct
  module StringLabel = struct
    type t = string
    let compare = Stdlib.compare
    let equal = (=)
    let hash = Hashtbl.hash
  end

  module EdgeKind = struct
    type t = edge_kind
    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal = (=)
    let default = PosEdge
  end

  include Imperative.Digraph.ConcreteLabeled(StringLabel)(EdgeKind)
end

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type stratification_error =
  | NegativeCycle of (string * edge_kind) list

let empty_graph = StringMap.empty

let add_edge from kind to_ g =
  DependencyGraph.add_edge_e g (DependencyGraph.E.create from kind to_)

let build_graph (prog : program) : DependencyGraph.t =
  let g = DependencyGraph.create () in

  let add_vertex p =
    if not (DependencyGraph.mem_vertex g p) then
      DependencyGraph.add_vertex g p
  in

  List.iter (fun clause ->
    match clause with
    | Fact p ->
        add_vertex p.name

    | Rule (head, body) ->
        add_vertex head.name;
        List.iter (fun lit ->
          let src, kind = match lit with
            | Pos p -> p.name, PosEdge
            | Neg p -> p.name, NegEdge
          in
          add_vertex src;
          DependencyGraph.add_edge_e g (DependencyGraph.E.create src kind head.name)
        ) body
  ) (clauses prog);

  g

let string_of_edge = function
  | PosEdge, p -> p
  | NegEdge, p -> "~" ^ p

let string_of_graph (graph : DependencyGraph.t) : string =
  let buffer = Buffer.create 128 in
  DependencyGraph.iter_vertex (fun v ->
    let edges = DependencyGraph.succ_e graph v in
    if edges <> [] then (
      let deps = 
        List.map (fun edge ->
          let dst = DependencyGraph.E.dst edge in
          let label = DependencyGraph.E.label edge in
          string_of_edge (label, dst)
        ) edges
      in
      Buffer.add_string buffer (Printf.sprintf "%s -> {%s}\n" v (String.concat ", " deps))
    )
  ) graph;
  Buffer.contents buffer

let find_labeled_cycles (graph : DependencyGraph.t) : ((string * edge_kind) list) list =
  let module SCC = Graph.Components.Make(DependencyGraph) in
  let sccs = SCC.scc_list graph in

  let rec dfs path visited node start incoming =
    if List.exists (fun (n, _) -> n = node) path then
      let cycle =
        List.rev ((node, incoming) :: List.take_while (fun (n, _) -> n <> node) path)
      in
      [cycle]
    else if StringSet.mem node visited then
      []
    else
      let visited = StringSet.add node visited in
      List.fold_left (fun acc edge ->
        let dst = DependencyGraph.E.dst edge in
        let label = DependencyGraph.E.label edge in
        acc @ dfs ((node, incoming) :: path) visited dst start label
      ) [] (DependencyGraph.succ_e graph node)
  in

  List.flatten (
    List.map (fun component ->
      match component with
      | [] -> []
      | nodes ->
          List.flatten (
            List.map (fun node -> dfs [] StringSet.empty node node PosEdge) nodes
          )
    ) sccs
  )

let check_stratification (graph : DependencyGraph.t) : (unit, stratification_error) result =
  let cycles = find_labeled_cycles graph in
  match List.find_opt (List.exists (fun (_, kind) -> kind = NegEdge)) cycles with
  | Some bad_cycle -> Error (NegativeCycle bad_cycle)
  | None -> Ok ()

let stratify (graph : DependencyGraph.t) : ((string -> int), stratification_error) result =
  match check_stratification graph with
  | Error err -> Error err
  | Ok () ->
      let module Topo = Graph.Topological.Make(DependencyGraph) in
      let stratum = ref StringMap.empty in

      DependencyGraph.iter_vertex (fun v ->
        stratum := StringMap.add v 0 !stratum
      ) graph;

      let order = Topo.fold (fun v acc -> v :: acc) graph [] |> List.rev in

      List.iter (fun from ->
        let from_stratum = StringMap.find from !stratum in
        DependencyGraph.iter_succ_e (fun edge ->
          let to_ = DependencyGraph.E.dst edge in
          let label = DependencyGraph.E.label edge in
          let to_stratum = StringMap.find to_ !stratum in
          let candidate =
            match label with
            | PosEdge -> from_stratum
            | NegEdge -> from_stratum + 1
          in
          if candidate > to_stratum then
            stratum := StringMap.add to_ candidate !stratum
        ) graph from
      ) order;

      Ok (fun pred -> StringMap.find_opt pred !stratum |> Option.value ~default:0)
