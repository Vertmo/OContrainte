open OCont_variable
open OCont_constraint
open OCont_domain

let rec solve vars constrs = match vars with
  | t::q -> if (isAssigned t)
    then solve q constrs
    else chooseValue (asList (domain t)) t q constrs
  | [] -> areConsistent constrs

and chooseValue values var vars constrs = match values with
  | [] -> unassign var; false
  | t::q -> assign var t; ((areConsistent constrs) && (solve vars constrs)) || (chooseValue q var vars constrs)
