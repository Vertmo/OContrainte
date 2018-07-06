open OCont_variable
open OCont_expression

type constr = BoolConstr of bool expr | AllDifferent of int expr list

let create e = e

let isConsistent c = match c with
  | BoolConstr b -> if allAssigned b then eval b else true
  | AllDifferent exprs ->
    List.for_all (fun e1 ->
        List.for_all (fun e2 -> (e1 == e2) ||
                                (not (allAssigned e1)) || (not (allAssigned e2)) ||
                                (eval e1 <> eval e2)) exprs) exprs

let areConsistent constrs = List.for_all isConsistent constrs

let propagate c = match c with
  | BoolConstr b -> propagate b
  | AllDifferent exprs -> let changed = ref false in
    List.iter (fun e1 ->
        if allAssigned e1 then let n = eval e1 in
          List.iter (fun e2 -> match e2 with
              | Var v2 when not (isAssigned v2) -> changed := (reduceDomain v2 n) || !changed
              | _ -> () (* TODO *)) exprs) exprs;
    !changed

let propagateAll constrs = List.fold_left (fun a c -> propagate c || a) false constrs
