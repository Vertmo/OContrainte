open OContrainte
open OContrainte.Expression
open OContrainte.Operators
open OContrainte.Constraint

let n = 200

let () =
  let dom = Domain.range 1 (n+1) in
  let vars = List.map (fun _ -> Variable.create dom) (Domain.asList dom) in
  let cstrs = ref [] in

  for i = 0 to n-2 do
    cstrs := (BoolConstr (Comparator ((~<), (Var (List.nth vars i)), (Var (List.nth vars (i+1))))))::!cstrs
  done;

  cstrs := (BoolConstr (Comparator ((~<>), (Var (List.nth vars (n-1))), (Var (List.hd vars)))))::!cstrs;

  print_endline "Debut de la propagation";
  if (Solver.propagate vars !cstrs)
  then print_endline "Yay on a trouvé :)"
  else print_endline "Ohhhh on n'a pas trouvé :("
