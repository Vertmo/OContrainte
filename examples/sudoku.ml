open OContrainte
open OContrainte.Expression

(** Get index for coordinates (x,y) in the grid *)
let index x y = y * 9 + x

(** Get index for frame i in block b *)
let blockIndex b i =
  let bx = (b mod 3) and by = (b / 3) in
  index (3 * bx + (i mod 3)) (3 * by + (i / 3))

(** Assign the frame at coordinates (x, y) to v *)
let assignFrame vars x y v = (Variable.assign (List.nth vars (index x y)) v)

let () =
  let d = Domain.range 1 (9+1) in
  let vars = List.map (fun _ -> Variable.create d) (Domain.asList (Domain.range 0 (9*9))) in
  let constrs = ref [] in

  (* Rows *)
  for i = 0 to 9-1 do
    for j = 0 to 9-1 do
      for k = 0 to j-1 do
        constrs := (Constraint.create (Comparator ((<>),
                                                   (Var (List.nth vars (index j i))),
                                                   (Var (List.nth vars (index k i))))))::!constrs
      done
    done
  done;

  (* Columns *)
  for i = 0 to 9-1 do
    for j = 0 to 9-1 do
      for k = 0 to j-1 do
        constrs := (Constraint.create (Comparator ((<>),
                                                   (Var (List.nth vars (index i j))),
                                                   (Var (List.nth vars (index i k))))))::!constrs
      done
    done
  done;

  (* Blocks *)
  for i = 0 to 9-1 do
    for j = 0 to 9-1 do
      for k = 0 to j-1 do
        constrs := (Constraint.create (Comparator ((<>),
                                                   (Var (List.nth vars (blockIndex i j))),
                                                   (Var (List.nth vars (blockIndex i k))))))::!constrs
      done
    done;
  done;

  (* Lets set some values ! *)
  (* Grid from : https://en.wikipedia.org/wiki/Sudoku#/media/File:Sudoku_Puzzle_by_L2G-20050714_standardized_layout.svg *)
  assignFrame vars 0 0 5; assignFrame vars 1 0 3; assignFrame vars 4 0 7;
  assignFrame vars 0 1 6; assignFrame vars 3 1 1; assignFrame vars 4 1 9; assignFrame vars 5 1 5;
  assignFrame vars 1 2 9; assignFrame vars 2 2 8; assignFrame vars 7 2 6;
  assignFrame vars 0 3 8; assignFrame vars 4 3 6; assignFrame vars 8 3 3;
  assignFrame vars 0 4 4; assignFrame vars 3 4 8; assignFrame vars 5 4 3; assignFrame vars 8 4 1;
  assignFrame vars 0 5 7; assignFrame vars 4 5 2; assignFrame vars 8 5 6;
  assignFrame vars 1 6 6; assignFrame vars 6 6 2; assignFrame vars 7 6 8;
  assignFrame vars 3 7 4; assignFrame vars 4 7 1; assignFrame vars 5 7 9; assignFrame vars 8 7 5;
  assignFrame vars 4 8 8; assignFrame vars 7 8 7; assignFrame vars 8 8 9;

  if not (Solver.solve vars !constrs)
  then print_endline "We didn't find a solution"
  else begin
    print_endline "We found a solution :";
    for i = 0 to 9-1 do
      for j = 0 to 9-1 do
        print_string " ";
        Variable.print_var (List.nth vars (9*i+j));
        print_string " ";
      done;
      print_endline ""
    done;
  end
