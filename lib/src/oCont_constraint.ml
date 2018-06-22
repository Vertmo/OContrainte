open OCont_variable
open OCont_expression

type constr = BoolConstr of boolExpr | AllDifferent of var list

let create e = e

let isConsistent c = match c with
  | BoolConstr b -> if allAssignedB b then evalB b else true
  | AllDifferent vars ->
    not (List.exists (fun v1 -> List.exists (fun v2 -> (not (v1 == v2)) && (not (value v1 = None)) && (v1 = v2)) vars) vars)

let areConsistent constrs = List.fold_left (fun a c -> a && isConsistent c) true constrs

let propagateNode c v = let changed = ref false in
  List.iter (fun n -> assign v n;
              if (not (isConsistent c)) then changed := reduceDomain v n || !changed;
              unassign v) (OCont_domain.asList (domain v));
  !changed


let propagateArcAux c v1 v2 = let changed = ref false in
  List.iter (fun n1 ->
      assign v1 n1;
      if not (List.exists
                 (fun n2 -> assign v2 n2; let cons = isConsistent c in unassign v2; cons) (OCont_domain.asList (domain v2)))
      then changed := reduceDomain v1 n1 || !changed;
      unassign v1)
    (OCont_domain.asList (domain v1));
  !changed

let propagateArc c v1 v2 = let changed = ref false in
  changed := propagateArcAux c v1 v2 || !changed;
  changed := propagateArcAux c v2 v1 || !changed;
  !changed

let propagate c = match c with
  | BoolConstr b -> (match List.filter (fun v -> not (isAssigned v)) (allVarsB b) with
    | v::[] -> propagateNode c v
    | v1::v2::[] -> propagateArc c v1 v2
    | _ -> false)
  | AllDifferent vars -> let changed = ref false in
    List.iter (fun v1 -> match value v1 with
        | Some n -> List.iter (fun v2 -> if v1 <> v2 then changed := (reduceDomain v2 n) || !changed) vars
        | None -> ()) vars;
    !changed

let propagateAll constrs = List.fold_left (fun a c -> propagate c || a) false constrs
