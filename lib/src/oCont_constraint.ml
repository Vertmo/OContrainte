open OCont_variable
open OCont_expression

type constr = boolExpr

let create e = e

let isConsistent c =
  let varRefs = allVarsB c in
  if List.fold_left (fun a -> fun v -> a && isAssigned !v) true varRefs
  then evalB c
  else true
