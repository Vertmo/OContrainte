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

let propagateNode c v = let changed = ref false in
  OCont_domain.iter (fun n -> assign v n;
              if (not (isConsistent c)) then changed := reduceDomain v n || !changed;
              unassign v) (domain v);
  !changed


let propagateArcAux c v1 v2 = let changed = ref false in
  OCont_domain.iter (fun n1 ->
      assign v1 n1;
      if not (OCont_domain.exists
                (fun n2 -> assign v2 n2; let cons = isConsistent c in unassign v2; cons) (domain v2))
      then changed := reduceDomain v1 n1 || !changed;
      unassign v1)
    (domain v1);
  !changed

let propagateArc c v1 v2 = let changed = ref false in
  changed := propagateArcAux c v1 v2 || !changed;
  changed := propagateArcAux c v2 v1 || !changed;
  !changed

let propagate c = match c with
  | BoolConstr b -> (match List.filter (fun v -> not (isAssigned v)) (allVars b) with
    | v::[] -> propagateNode c v
    | v1::v2::[] -> propagateArc c v1 v2
    | _ -> false)
  | AllDifferent exprs -> let changed = ref false in
    List.iter (fun e1 ->
        if allAssigned e1 then let n = eval e1 in
          List.iter (fun e2 -> match e2 with
              | Var v2 when not (isAssigned v2) -> changed := (reduceDomain v2 n) || !changed
              | _ -> ()) exprs) exprs;
    !changed

let propagateAll constrs = List.fold_left (fun a c -> propagate c || a) false constrs
