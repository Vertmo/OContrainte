open Avr
open OContrainte
open OContrainte.Expression


let g = PIN11 and r = PIN10

let d = Domain.fromList [0;1]
let v = Variable.create d
let c = Constraint.create (Comparator ((<), (Var v), (IntConst 1)))

let () =
  if (Solver.solve [v] [c])
  then digital_write g HIGH
  else digital_write r HIGH
