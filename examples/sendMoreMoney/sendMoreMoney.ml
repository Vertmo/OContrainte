open Avr
open OContrainte
open OContrainte.Expression

(** https://en.wikipedia.org/wiki/Verbal_arithmetic *)

let green = PIN11 and red = PIN10

let () =
  let _ = digital_read PIN7 in
  let dom = Domain.range 0 10 in
  let s = Variable.create dom and e = Variable.create dom and n = Variable.create dom
  and d = Variable.create dom and m = Variable.create dom and o = Variable.create dom
  and r = Variable.create dom and y = Variable.create dom in

  let vars = [s;e;n;d;m;o;r;y] in
  let constrs = ref [] in

  (* Most significative numbers (m and s) are not 0 *)
  constrs := (Constraint.BoolConstr (Comparator ((<>), (Var m), (IntConst 0))))::!constrs;
  constrs := (Constraint.BoolConstr (Comparator ((<>), (Var s), (IntConst 0))))::!constrs;

  (* Each letter has a different value *)
  constrs := (Constraint.AllDifferent vars)::!constrs;

  (* Sum *)
  let send = List.map (fun v -> Var v) [s;e;n;d]
  and more = List.map (fun v -> Var v) [m;o;r;e]
  and money = List.map (fun v -> Var v)[m;o;n;e;y] in
  let sumBaseTen = (List.fold_left (fun a -> fun e -> 10*a+e) 0) in
  constrs := (Constraint.BoolConstr (Comparator ((=),
                                             (IntBinOp ((+),
                                                        (IntMultiOp (sumBaseTen, send)),
                                                        (IntMultiOp (sumBaseTen, more)))),
                                             (IntMultiOp (sumBaseTen, money)))))::!constrs;

  if Solver.solve vars !constrs
  then digital_write green HIGH
  else digital_write red HIGH
