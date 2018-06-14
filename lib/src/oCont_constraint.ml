open OCont_variable
open OCont_expression

type constr = BoolConstr of boolExpr | AllDifferent of var list

let create e = e

let isConsistent c = match c with
  | BoolConstr b -> if allAssignedB b then evalB b else true
  | AllDifferent vars ->
    not (List.exists (fun v1 -> List.exists (fun v2 -> (not (v1 == v2)) && (not (value v1 = None)) && (v1 = v2)) vars) vars)

let areConsistent constrs = List.fold_left (fun a c -> a && isConsistent c) true constrs

let propagate c = match c with
  | BoolConstr b -> false (* TODO *)
  | AllDifferent vars -> let changed = ref false in
    List.iter (fun v -> match value v with
        | Some n -> List.iter (fun v -> changed := (reduceDomain v n)|| !changed) vars
        | None -> ()) vars; !changed

let propagateAll constrs = List.fold_left (fun a c -> propagate c || a) false constrs
