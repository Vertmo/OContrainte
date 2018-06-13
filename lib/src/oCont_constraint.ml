open OCont_variable
open OCont_expression

type constr = BoolConstr of boolExpr | AllDifferent of var list

let create e = e

let isConsistent c = match c with
  | BoolConstr b -> if allAssignedB b then evalB b else true
  | AllDifferent vars ->
    let consistent = ref true and i = ref 0 in
    while !consistent && !i < List.length vars do
      let j = ref 0 in
      while !consistent && !j < !i do
        consistent := !consistent && not ((value (List.nth vars !i) <> None) && (value (List.nth vars !i) = value (List.nth vars !j)));
        j := !j + 1
      done;
      i := !i + 1
    done;
    !consistent

let areConsistent constrs = List.fold_left (fun a -> fun c -> a && isConsistent c) true constrs
