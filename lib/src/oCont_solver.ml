open OCont_variable
open OCont_constraint
open OCont_domain

let propagate vars constrs =
  List.iter (fun v ->
      if (card (domain v)) = 1 then match (min (domain v)) with
        | Some n -> assign v n
        | None -> ()) vars;(* assign v (min (domain v))) vars; *)

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
  | t::q -> let reus = exists (domain t) (fun v -> assign t v;
                                     (areConsistent constrs) && (backtrack q constrs)) in
    if reus then true else (unassign t; false)
  | [] -> areConsistent constrs

let solve vars constrs = (propagate vars constrs) || (backtrack vars constrs)
