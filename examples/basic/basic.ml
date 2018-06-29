open Avr
open OContrainte
open OContrainte.Expression


let () =
  let g = PIN11 and r = PIN10 in
  pin_mode g OUTPUT; digital_write g HIGH;
  pin_mode r OUTPUT; digital_write r HIGH;
  let d = Domain.range 0 2 in
  let v1 = Variable.create d and v2 = Variable.create d in
  let c1 = Constraint.BoolConstr (Comparator ((<), (Var v1), (Const 1))) and
  c2 = Constraint.AllDifferent([v1;v2]) in

  if (Solver.solve [v1;v2] [c1;c2])
  then digital_write g LOW
  else digital_write r LOW
