open OContrainte
open OContrainte.Expression

let%component GreenLed = Circuits.MakeLed(connectedPin = PIN0)
let%component RedLed = Circuits.MakeLed(connectedPin = PIN1)

let n = 15

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

  Serial.write("Début...");
  let startTime = millis () in
  if Solver.solve vars !constrs
  then (
    Serial.write ("Fini en "^(string_of_int (millis () - startTime))^"ms");
    GreenLed.on ()
  )
  else RedLed.on ()
