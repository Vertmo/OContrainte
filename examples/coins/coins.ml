open Avr
open OContrainte
open OContrainte.Domain
open OContrainte.Operators
open OContrainte.Expression
open OContrainte.Constraint

let g = PIN22 and r = PIN24

(* Give the change back ! *)

let n = 13 (* Amount to give back *)
let m = 3 (* Maximum number of coins *)

let coinValues = [1;2;5;10] (* Values of the coins *)
let doms = [range 0 4; range 0 2; range 0 3; range 0 2] (* Number of available coins of each value *)

let () =
  pin_mode g OUTPUT; digital_write g LOW;
  pin_mode r OUTPUT; digital_write r LOW;

  let v = List.map (fun d -> Variable.create d) doms in

  let vars = List.map (fun v -> (Var v)) v and
  varsTimesValues = List.map2 (fun v value -> (BinOp ((fun x y -> x * y), (Var v), (Const value)))) v coinValues in
  let cstrs = [BoolConstr (Comparator ((~=), (MultiOp ((List.fold_left (+) 0), varsTimesValues)), (Const n)));
              BoolConstr (Comparator ((~<=), (MultiOp ((List.fold_left (+) 0), vars)), (Const m)))] in

  if (Solver.solve v cstrs)
  then digital_write g HIGH
  else digital_write r HIGH
