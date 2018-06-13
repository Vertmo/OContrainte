open OUnit2
open OContrainte
open OContrainte.Expression

let testSolve1 test_ctxt =
  let cstr = Constraint.BoolConstr (Comparator ((<),
                                            (IntConst 1),
                                            (IntConst 2))) in
  assert_equal (Solver.solve [] [cstr]) true

let testSolve2 test_ctxt =
  let cstr1 = Constraint.BoolConstr (Comparator ((<),
                                             (IntConst 1),
                                             (IntConst 2))) in
  let cstr2 = Constraint.BoolConstr (Comparator ((>),
                                             (IntConst 1),
                                             (IntConst 2))) in
  assert_equal (Solver.solve [] [cstr1; cstr2]) false

let testSolve3 test_ctxt =
  let var1 = Variable.create (Domain.fromList [1;2;3;4;5]) in
  let var2 = Variable.create (Domain.fromList [4;5;7]) in
  let cstr1 = Constraint.BoolConstr (Comparator ((>),
                                             (Var var2),
                                             (IntConst 6))) in
  let cstr2 = Constraint.BoolConstr (Comparator ((=),
                                             (IntBinOp ((+),
                                                        (Var var1),
                                                        (Var var2))),
                                             (IntConst 10))) in
  assert_equal (Solver.solve [var1; var2] [cstr1; cstr2]) true;
  assert_equal (Variable.value var1) (Some 3);

  Variable.unassign var2;
  Variable.assign var1 5;
  assert_equal (Solver.solve [var1; var2] [cstr1; cstr2]) false


let suite = [
  "solve1">::testSolve1;
  "solve2">::testSolve2;
  "solve3">::testSolve3;
]
