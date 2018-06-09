open OContrainte
open OContrainte.Expression

let () =
  let dom = Domain.range 0 2 in
  let v = Variable.create dom in

  let constrs = [(Constraint.create (Comparator ((=), (Var v), (IntConst 1))))] in
  if not (Solver.solve [v] constrs) (* y'a une instruction 62 qui fait tout foirer ! *)
  then ()
  else ()
