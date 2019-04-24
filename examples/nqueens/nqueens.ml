open OContrainte
open OContrainte.Expression

let%component GreenLed = Circuits.MakeLed(connectedPin = PIN0)
let%component RedLed = Circuits.MakeLed(connectedPin = PIN1)

let n = 5

let () =
  GreenLed.init (); RedLed.init ();
  GreenLed.off (); RedLed.off ();

  let d = Domain.range 0 (n-1) in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList d) in
  let constrs = ref [] in

  (* Queens are on different rows *)
  constrs := (Constraint.AllDifferent (List.map (fun v -> Var v) vars))::!constrs;

  (* Queens are on different diagonals *)
  constrs := (Constraint.AllDifferent (List.mapi (fun i v -> (BinOp ((+),(Var v),(Const i)))) vars))::!constrs;
  constrs := (Constraint.AllDifferent (List.mapi (fun i v -> (BinOp ((-),(Var v),(Const i)))) vars))::!constrs;

  (* Variable.assign (List.nth vars 0) 3; *)
  if Solver.backtrack vars !constrs
  then GreenLed.on ()
  else RedLed.on ()
