open Avr
open OContrainte
open OContrainte.Expression

let n = 8

let () =
  let g = PIN11 and r = PIN10 and b = PIN9 in
  pin_mode b OUTPUT; digital_write b LOW;
  pin_mode g OUTPUT; digital_write g HIGH;
  pin_mode r OUTPUT; digital_write r HIGH;
  let d = Domain.range 0 n in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  constrs := (Constraint.AllDifferent vars)::!constrs;

  (* Diagonals *)
  for i = 1 to n-1 do
    for j = 0 to i-1 do
      constrs := (Constraint.BoolConstr
                    (BoolBinOp ((&&),
                                (Comparator ((<>),
                                             (Var (List.nth vars i)),
                                             (IntBinOp ((+),
                                                        (Var (List.nth vars j)),
                                                        (IntConst (i - j)))))),
                                (Comparator ((<>),
                                             (Var (List.nth vars i)),
                                             (IntBinOp ((-),
                                                        (Var (List.nth vars j)),
                                                        (IntConst (i - j)))))))))::!constrs
    done
  done;
  (*Variable.assign (List.nth vars 0) 3;*)
  if Solver.solve vars !constrs
  then (digital_write g LOW; digital_write b HIGH)
  else (digital_write r LOW; digital_write b HIGH)
