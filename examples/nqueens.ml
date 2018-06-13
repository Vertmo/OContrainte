open OContrainte
open OContrainte.Expression

let n = 8

let () =
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
  if not (Solver.solve vars !constrs)
  then print_endline "We didn't find a solution..."
  else begin
    print_endline "We found a solution :";
    for i = 0 to n-1 do
      print_string "["; print_int i; print_string ",";
      Variable.print_var (List.nth vars i); print_endline "]"
    done
  end
