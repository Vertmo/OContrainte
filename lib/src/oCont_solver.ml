open OCont_variable
open OCont_constraint
open OCont_domain

let rec solve vars constrs = match vars with
  | t::q when (isAssigned t) -> solve q constrs
  | t::q -> let reus = List.exists (fun v -> assign t v;
                                     (areConsistent constrs) && (solve vars constrs)) (asList (domain t)) in
    if reus then true else (unassign t; false)
    (*then solve q constrs
      else chooseValue (asList (domain t)) t q constrs*)
  | [] -> areConsistent constrs

and chooseValue values var vars constrs = match values with
  | [] -> unassign var; false
  | t::q -> assign var t; ((areConsistent constrs) && (solve vars constrs)) || (chooseValue q var vars constrs)
