open Avr
open OContrainte
open OContrainte.Expression

let n = 6

let () =
  let g = PIN22 and r = PIN24 and b = PIN26 in
  pin_mode b OUTPUT; digital_write b HIGH;
  pin_mode g OUTPUT; digital_write g LOW;
  pin_mode r OUTPUT; digital_write r LOW;
  let d = Domain.range 0 n in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  constrs := (Constraint.AllDifferent vars)::!constrs;

  (* Diagonals *)
  for i = 1 to n-1 do
    for j = 0 to i-1 do
      constrs := (Constraint.BoolConstr
                    (BinOp ((&&),
                                (Comparator ((<>),
                                             (Var (List.nth vars i)),
                                             (BinOp ((+),
                                                        (Var (List.nth vars j)),
                                                        (Const (i - j)))))),
                                (Comparator ((<>),
                                             (Var (List.nth vars i)),
                                             (BinOp ((-),
                                                        (Var (List.nth vars j)),
                                                        (Const (i - j)))))))))::!constrs
    done
  done;
  (* Variable.assign (List.nth vars 0) 3; *)
  if Solver.backtrack vars !constrs
  then (digital_write g HIGH; digital_write b LOW)
  else (digital_write r HIGH; digital_write b LOW)
