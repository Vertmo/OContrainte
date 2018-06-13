open Avr
open OContrainte
open OContrainte.Expression

let g = PIN11 and r = PIN10

let () =
  let _ = digital_read PIN7 in
  let d = Domain.range 0 2 in
  let v = Variable.create d in
  let c = Constraint.BoolConstr (Comparator ((<), (Var v), (IntConst 1))) in

  if (Solver.solve [v] [c])
  then digital_write g HIGH
  else digital_write r HIGH
