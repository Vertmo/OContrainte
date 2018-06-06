open OContrainte
open OContrainte.Expression

(** https://en.wikipedia.org/wiki/Verbal_arithmetic *)

let () =
  let dom = Domain.range 0 10 in
  let s = Variable.create dom and e = Variable.create dom and n = Variable.create dom
  and d = Variable.create dom and m = Variable.create dom and o = Variable.create dom
  and r = Variable.create dom and y = Variable.create dom in

  let vars = [s;e;n;d;m;o;r;y] in
  let constrs = ref [] in

  (* Most significative numbers (m and s) are not 0 *)
  constrs := (Constraint.create (Comparator ((<>), (Var m), (IntConst 0))))::!constrs;
  constrs := (Constraint.create (Comparator ((<>), (Var s), (IntConst 0))))::!constrs;

  (* Each letter has a different value *)
  for i = 0 to (List.length vars - 1) do
    for j = 0 to i-1 do
      constrs := (Constraint.create (Comparator ((<>),
                                                 (Var (List.nth vars i)),
                                                 (Var (List.nth vars j)))))::!constrs;
    done
  done;

  (* Sum *)
  let send = List.map (fun v -> Var v) [s;e;n;d]
  and more = List.map (fun v -> Var v) [m;o;r;e]
  and money = List.map (fun v -> Var v)[m;o;n;e;y] in
  let sumBaseTen = (List.fold_left (fun a -> fun e -> 10*a+e) 0) in
  constrs := (Constraint.create (Comparator ((=),
                                             (IntBinOp ((+),
                                                        (IntMultiOp (sumBaseTen, send)),
                                                        (IntMultiOp (sumBaseTen, more)))),
                                             (IntMultiOp (sumBaseTen, money)))))::!constrs;

  if not (Solver.solve vars !constrs)
  then print_endline "We didn't find a solution..."
  else begin
    print_endline "We found a solution :";
    print_string "    "; Variable.print_var s;
    print_string " "; Variable.print_var e;
    print_string " "; Variable.print_var n;
    print_string " "; Variable.print_var d;
    print_endline "";

    print_string "+   "; Variable.print_var m;
    print_string " "; Variable.print_var o;
    print_string " "; Variable.print_var r;
    print_string " "; Variable.print_var e;
    print_endline "";

    print_endline "-----------";

    print_string "= "; Variable.print_var m;
    print_string " "; Variable.print_var o;
    print_string " "; Variable.print_var n;
    print_string " "; Variable.print_var e;
    print_string " "; Variable.print_var y;
    print_endline "";
  end

