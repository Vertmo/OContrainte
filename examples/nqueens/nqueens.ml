open Avr
open OContrainte
open OContrainte.Expression

let n = 5

let () =
  let g = PIN22 and r = PIN24 and b = PIN26 in
  pin_mode b OUTPUT; digital_write b HIGH;
  pin_mode g OUTPUT; digital_write g LOW;
  pin_mode r OUTPUT; digital_write r LOW;
  let d = Domain.range 0 n in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  constrs := (Constraint.AllDifferent (List.map (fun v -> Var v) vars))::!constrs;

  (* Queens are on different diagonals *)
  constrs := (Constraint.AllDifferent (List.mapi (fun i v -> (BinOp ((+),(Var v),(Const i)))) vars))::!constrs;
  constrs := (Constraint.AllDifferent (List.mapi (fun i v -> (BinOp ((-),(Var v),(Const i)))) vars))::!constrs;

  (* Variable.assign (List.nth vars 0) 3; *)
  if Solver.backtrack vars !constrs
  then (digital_write g HIGH; digital_write b LOW)
  else (digital_write r HIGH; digital_write b LOW)
