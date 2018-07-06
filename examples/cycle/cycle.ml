open Avr
open OContrainte
open OContrainte.Expression
open OContrainte.Constraint

let n = 3

let () =
  let g = PIN22 and r = PIN24 in
  pin_mode g OUTPUT; digital_write g LOW;
  pin_mode r OUTPUT; digital_write r LOW;
  let dom = Domain.range 1 (n+1) in
  let vars = List.map (fun _ -> Variable.create dom) (Domain.asList dom) in
  let cstrs = ref [] in

  for i = 0 to n-2 do
    cstrs := (BoolConstr (Comparator ((<), (Var (List.nth vars i)), (Var (List.nth vars (i+1))))))::!cstrs
  done;

  cstrs := (BoolConstr (Comparator ((<>), (Var (List.nth vars (n-1))), (Var (List.hd vars)))))::!cstrs;

  if (Solver.propagate vars !cstrs)
  then digital_write g HIGH
  else digital_write r HIGH
