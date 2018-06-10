open Avr
open OContrainte
open OContrainte.Expression


let g = PIN11 and r = PIN10

let () =
  let d = Domain.fromArray [|1;2|] in
  let v = Variable.create d in
  let c = Constraint.create (Comparator ((<>), (Var v), (IntConst 2))) in

  if (Solver.solve [v] [c])
  then digital_write g HIGH
  else digital_write r HIGH
