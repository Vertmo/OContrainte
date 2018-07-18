open OContrainte
open OContrainte.Expression

let n = 8

let () =
  let d = Domain.range 0 (n-1) in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  constrs := (Constraint.AllDifferent (List.map (fun v -> Var v) vars))::!constrs;

  (* Queens are on different diagonals *)
  constrs := (Constraint.AllDifferent (List.mapi (fun i v -> (BinOp ((+),(Var v),(Const i)))) vars))::!constrs;
  constrs := (Constraint.AllDifferent (List.mapi (fun i v -> (BinOp ((-),(Var v),(Const i)))) vars))::!constrs;

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
