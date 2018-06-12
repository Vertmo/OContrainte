open Avr
open OContrainte
open OContrainte.Expression

let g = PIN11 and r = PIN10
let n = 8

let () =
  let d = Domain.range 0 n in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  for i = 0 to n-1 do
    for j = 0 to i-1 do
      constrs := (Constraint.create
                    (Comparator ((<>),
                                 (Var (List.nth vars i)),
                                 (Var (List.nth vars j)))))::!constrs
    done
  done;

  (* Diagonals *)
  for i = 1 to n-1 do
    for j = 0 to i-1 do
      constrs := (Constraint.create
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
  then digital_write g HIGH
  else digital_write r HIGH
