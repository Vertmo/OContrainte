(* https://en.wikipedia.org/wiki/Golomb_ruler *)

open OContrainte
open OContrainte.Domain
open OContrainte.Expression
open OContrainte.Constraint

let order = 7 and length = 25 (* We want to find the optimal golomb rulers, but we already know it's order *)

let () =
  let dom = range 0 length in
  let vars = List.init order (fun _ -> Variable.create dom) in

  let pairs = List.fold_left (fun a v1 -> a@(List.fold_left (fun a v2 -> if v1 == v2 then a else (v1, v2)::a) [] vars)) [] vars in

  let subs = List.map (fun (v1, v2) -> (BinOp ((-), (Var v1), (Var v2)))) pairs in

  let cstrs = [AllDifferent subs] in

  if Solver.solve vars cstrs
  then (
    print_endline "We found a solution :";
    List.iter (fun v -> Variable.print_var v; print_endline "") vars
  ) else print_endline "No solution found..."
