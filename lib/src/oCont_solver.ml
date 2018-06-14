open OCont_variable
open OCont_constraint
open OCont_domain

let propagate vars constrs = true

let rec backtrack vars constrs = match vars with
  | t::q when (isAssigned t) -> backtrack q constrs
  | t::q -> let reus = List.exists (fun v -> assign t v;
                                     (areConsistent constrs) && (backtrack vars constrs)) (asList (domain t)) in
    if reus then true else (unassign t; false)
  | [] -> areConsistent constrs

let solve vars constrs = backtrack vars constrs
