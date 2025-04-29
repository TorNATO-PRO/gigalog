open Ast

let rec eval_expr theta = function
  | EConst n -> Some n
  | EVar v -> (
      match List.assoc_opt v theta with
      | Some (Int n) -> Some n
      | _ -> None
    )
  | ENeg e -> (
      match eval_expr theta e with
      | Some n -> Some (-n)
      | None -> None
    )
  | EAdd (l, r) -> (
      match eval_expr theta l, eval_expr theta r with
      | Some n1, Some n2 -> Some (n1 + n2)
      | _ -> None
    )
  | ESub (l, r) -> (
      match eval_expr theta l, eval_expr theta r with
      | Some n1, Some n2 -> Some (n1 - n2)
      | _ -> None
    )
  | EMul (l, r) -> (
      match eval_expr theta l, eval_expr theta r with
      | Some n1, Some n2 -> Some (n1 * n2)
      | _ -> None
    )
  | EDiv (l, r) -> (
      match eval_expr theta l, eval_expr theta r with
      | Some _ , Some 0 -> None
      | Some n1, Some n2 -> Some (n1 / n2)
      | _ -> None
    )
  | EPow (l, r) -> (
      match eval_expr theta l, eval_expr theta r with
      | Some n1, Some n2 -> Some (int_of_float ((float_of_int n1) ** (float_of_int n2)))
      | _ -> None
    )
