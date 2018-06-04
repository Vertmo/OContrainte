open OContrainte
open OContrainte.Expression

let () =
  let n = 8 in
  let vars = List.map (fun _ -> Variable.create (Domain.range 0 n)) (Domain.asList (Domain.range 0 n)) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  for i = 1 to n-1 do
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
  if not (Solver.solve vars !constrs)
  then print_endline "We didn't find a solution..."
  else begin
    print_endline "We found a solution, here it is :";
    for i = 0 to n-1 do
      print_string "["; print_int i; print_string ",";
      match (Variable.value (List.nth vars i)) with
      |Some n -> print_int n; print_endline "]"
      |None -> print_int 0; print_endline "]"
    done
  end
