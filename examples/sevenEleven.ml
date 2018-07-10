open OContrainte
open OContrainte.Domain
open OContrainte.Expression
open OContrainte.Constraint

let () =
  let a = Variable.create (range 0 330) and b = Variable.create (range 0 160)
  and c = Variable.create (range 0 140) and d = Variable.create (range 0 140) in

  let vars = [(Var a); (Var b); (Var c); (Var d)] in

  let cstrs = ref [] in

  let sum = List.fold_left (+) 0 and product = List.fold_left (fun x y -> x * y) 1 in
  cstrs := (BoolConstr (Comparator ((=), (MultiOp (sum, vars)), (Const 711))))::!cstrs;
  cstrs := (BoolConstr (Comparator ((=), (MultiOp (product, vars)), (Const 711000000))))::!cstrs;

  if Solver.solve [a;b;c;d] !cstrs
  then (
    print_endline "We found a solution :";
    print_string "a :"; Variable.print_var a; print_endline "";
    print_string "b :"; Variable.print_var b; print_endline "";
    print_string "c :"; Variable.print_var c; print_endline "";
    print_string "d :"; Variable.print_var d; print_endline "";
  )
  else print_endline "No solution found..."
