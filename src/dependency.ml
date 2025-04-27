open Ast

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type edge_kind = PosEdge | NegEdge

type dependency_graph = (edge_kind * string) list StringMap.t

let empty_graph = StringMap.empty

let add_edge from kind to_ g =
  let edges = Option.value ~default:[] (StringMap.find_opt from g) in
  StringMap.add from ((kind, to_) :: edges) g

let build_graph (prog : program) : dependency_graph =
  List.fold_left (fun g clause ->
    match clause with
    | Fact _ -> g
    | Rule (head, body) ->
        List.fold_left (fun g lit ->
          match lit with
          | Pos p -> add_edge head.name PosEdge p.name g
          | Neg p -> add_edge head.name NegEdge p.name g
        ) g body
  ) empty_graph prog

let string_of_edge = function
  | PosEdge, p -> p
  | NegEdge, p -> "~" ^ p

let string_of_graph (graph : dependency_graph) : string =
  StringMap.bindings graph
  |> List.map (fun (pred, deps) ->
      Printf.sprintf "%s -> {%s}" pred
        (String.concat ", " (List.map string_of_edge deps))
    )
  |> String.concat "\n"

let find_cycles (graph : dependency_graph) : (string list) list =
  let rec dfs path visited node =
    if List.mem node path then
      [[node] @ (List.take_while (fun n -> n <> node) path)]
    else if StringSet.mem node visited then
      []
    else
      let visited = StringSet.add node visited in
      match StringMap.find_opt node graph with
      | None -> []
      | Some deps ->
          List.fold_left (fun acc (_, dep) ->
            acc @ (dfs (node :: path) visited dep)
          ) [] deps
  in
  StringMap.fold (fun node _ acc ->
    acc @ (dfs [] StringSet.empty node)
  ) graph []

let check_stratification (graph : dependency_graph) : (unit, string) result =
  let cycles = find_cycles graph in
  let detect_negative_in_cycle cycle =
    let rec has_negative_edge = function
      | x :: y :: rest ->
          let edges = Option.value ~default:[] (StringMap.find_opt x graph) in
          if List.exists (fun (kind, tgt) -> kind = NegEdge && tgt = y) edges
          then true
          else has_negative_edge (y :: rest)
      | _ -> false
    in
    has_negative_edge cycle
  in
  match List.find_opt detect_negative_in_cycle cycles with
  | Some bad_cycle ->
      Error (Printf.sprintf "Negative cycle detected: %s"
               (String.concat " -> " bad_cycle))
  | None ->
      Ok ()

let stratify (g : dependency_graph) : string -> int =
  (* build initial map containing **all** predicates *)
  let strata =
    StringMap.fold
      (fun p edges acc ->
        let acc = StringMap.update p (function _ -> Some 0) acc in
        List.fold_left
          (fun acc (_, q) -> StringMap.update q (function _ -> Some 0) acc)
          acc edges)
      g StringMap.empty
  in
  (* iterative relaxation *)
  let rec relax changed strata =
    let changed, strata =
      StringMap.fold (fun p edges (changed, s) ->
        List.fold_left (fun (changed, s) (k, q) ->
          let p_str = Option.value ~default:0 (StringMap.find_opt p s) in
          let q_str = Option.value ~default:0 (StringMap.find_opt q s) in
          let wanted =
            match k with
            | PosEdge -> q_str
            | NegEdge -> q_str + 1
          in
          if wanted > p_str then
            (true, StringMap.add p wanted s)
          else
            (changed, s)
        ) (changed, s) edges
      ) g (changed, strata)
    in
    if changed then relax false strata else strata
  in
  let final = relax false strata in
  (* lookup function *)
  fun p -> Option.value ~default:0 (StringMap.find_opt p final)
