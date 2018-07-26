open OCont_variable
open OCont_constraint
open OCont_domain

let propagate vars constrs =
  List.iter (fun v -> setDomain v (domain v)) vars;

  List.iter (fun v ->
      match value v with
      | None -> ()
      | Some n -> (List.iter (fun value -> (let _ = (reduceDomain v value) in ())) (List.filter (fun value -> value <> n) (asList (domain v))))) vars;

  let changed = ref true in
  while !changed do
    changed := OCont_constraint.propagateAll constrs
  done;
  List.for_all isAssigned vars

let rec backtrack vars constrs = match vars with
  | t::q when (isAssigned t) -> backtrack q constrs
  | t::q -> let reus = exists (fun v -> assign t v;
                                (areConsistent constrs) && (backtrack q constrs)) (domain t) in
    if reus then true else (unassign t; false)
  | [] -> areConsistent constrs

let chooseVar vars = match vars with
  | [] -> (None, [])
  | t::q -> (Some t, q)

let rec solve vars constrs = match vars with
  | [] -> areConsistent constrs
  | _ -> ((propagate vars constrs) && areConsistent constrs) ||
         let (v, vars) = chooseVar vars in match v with
         | Some v -> let backup = List.map copy vars in
           exists (fun n -> assign v n; if (areConsistent constrs) && (solve vars constrs) then true else (List.iter2 resetFromCopy vars backup; false)) (domain v)
         | None -> false
