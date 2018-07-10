(*                                Graph coloring                                   *)
(* Example taken from : https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_1.html *)

open OContrainte
open OContrainte.Domain
open OContrainte.Expression
open OContrainte.Constraint

(* Is there a k-coloring ? (in this case it is only 4-colorable) *)
let k = 4

let adjacent l n1 n2 =
  BoolConstr (Comparator ((<>), (Var (List.nth l (n1-1))), (Var (List.nth l (n2-1)))))

let () =
  let dom = range 1 (k+1) in
  let vars = List.init 5 (fun _ -> Variable.create dom) in

  let cstrs = [(adjacent vars 1 2);
               (adjacent vars 1 3);
               (adjacent vars 1 4);
               (adjacent vars 1 5);
               (adjacent vars 2 3);
               (adjacent vars 2 4);
               (adjacent vars 3 4);
               (adjacent vars 4 5)] in

  if Solver.solve vars cstrs
  then print_endline "Found a solution !"
  else print_endline "Did not find a solution..."
