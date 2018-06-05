open OCont_variable
open OCont_expression

type constr = boolExpr

let create e = e

let isConsistent c =
  if allAssignedB c
  then evalB c
  else true

let areConsistent constrs = List.fold_left (fun a -> fun c -> a && isConsistent c) true constrs
