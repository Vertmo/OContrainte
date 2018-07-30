open Avr
open OContrainte
open OContrainte.Operators
open OContrainte.Expression

let green = PIN22 and red = PIN24

(** https://en.wikipedia.org/wiki/Verbal_arithmetic *)

let () =
  pin_mode green OUTPUT; digital_write green LOW;
  pin_mode red OUTPUT; digital_write red LOW;

  let dom = Domain.range 0 9 in
  let a = Variable.create dom and b = Variable.create dom and c = Variable.create dom in

  let vars = [a;b;c] in
  let constrs = ref [] in

  (* Most significative numbers (t, a and c) are not 0 *)
  constrs := (Constraint.BoolConstr (Comparator ((~<>), (Var a), (Const 0))))::!constrs;
  constrs := (Constraint.BoolConstr (Comparator ((~<>), (Var c), (Const 0))))::!constrs;

  (* Each letter has a different value *)
  constrs := (Constraint.AllDifferent (List.map (fun v -> Var v) vars))::!constrs;

  (* Sum *)
  let ab = List.map(fun v -> Var v) [a;b]
  and ac = List.map (fun v -> Var v) [a;c]
  and cba = List.map (fun v -> Var v) [c;b;a] in
  let sumBaseTen = (List.fold_left (fun a e -> 10*a+e) 0) in
  constrs := (Constraint.BoolConstr (Comparator ((~=),
                                                 (BinOp ((+),
                                                         (MultiOp (sumBaseTen, ab)),
                                                         (MultiOp (sumBaseTen, ac)))),
                                                 (MultiOp (sumBaseTen, cba)))))::!constrs;

  if Solver.solve vars !constrs
  then digital_write green HIGH
  else digital_write red HIGH
