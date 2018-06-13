open OContrainte
open OContrainte.Expression

let n = 4

let () =
  let d = Domain.range 1 (n*n+1) in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let magicTotal = ((n*n*n) + n)/2 in
  let constrs = ref [] in

  (* All variables are different *)
  constrs := (Constraint.AllDifferent vars)::!constrs;

  (* Rows equal to magicTotal *)
  for i = 0 to n-1 do
    let row = List.map (fun j -> (List.nth vars (i*n+j))) (Domain.asList (Domain.range 0 n)) in
    constrs := (Constraint.BoolConstr (Comparator ((=),
                                               (IntMultiOp ((List.fold_left (+) 0),
                                                            (List.map (fun v -> Var v) row))),
                                               (IntConst magicTotal))))::!constrs
    done;

  (* Columns equal to magicTotal *)
  for i = 0 to n-1 do
    let column = List.map (fun j -> (List.nth vars (j*n+i))) (Domain.asList (Domain.range 0 n)) in
    constrs := (Constraint.BoolConstr (Comparator ((=),
                                               (IntMultiOp ((List.fold_left (+) 0),
                                                            (List.map (fun v -> Var v) column))),
                                               (IntConst magicTotal))))::!constrs
  done;

  (* Diagonals equal to magicTotal *)
  let diag1 = List.map (fun i -> (List.nth vars (i*n+i))) (Domain.asList (Domain.range 0 n))
  and diag2 = List.map (fun i -> (List.nth vars (i*n + (n-1) - i))) (Domain.asList (Domain.range 0 n)) in

  constrs := (Constraint.BoolConstr (BoolBinOp ((&&),
                                            (Comparator ((=),
                                                         (IntMultiOp ((List.fold_left (+) 0),
                                                                      (List.map (fun v -> Var v) diag1))),
                                                         (IntConst magicTotal))),
                                            (Comparator ((=),
                                                         (IntMultiOp ((List.fold_left (+) 0),
                                                                      (List.map (fun v -> Var v) diag2))),
                                                         (IntConst magicTotal))))))::!constrs;

  if not (Solver.solve vars !constrs)
  then print_endline "We didn't find a solution..."
  else begin
    print_endline "We found a solution :";
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        print_string " ";
        match (Variable.value (List.nth vars (n*i+j))) with
        | Some v when v < 10 -> print_int v; print_string "  "
        | Some v -> print_int v; print_string " "
        | None -> print_string "? ";
      done;
      print_endline ""
    done
  end
