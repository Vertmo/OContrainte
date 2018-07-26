open OUnit2
open OContrainte
open OContrainte.Expression

let testBacktrack1 test_ctxt =
  let cstr = Constraint.BoolConstr (Comparator ((<),
                                            (Const 1),
                                            (Const 2))) in
  assert_equal (Solver.backtrack [] [cstr]) true

let testBacktrack2 test_ctxt =
  let cstr1 = Constraint.BoolConstr (Comparator ((<),
                                             (Const 1),
                                             (Const 2))) in
  let cstr2 = Constraint.BoolConstr (Comparator ((>),
                                             (Const 1),
                                             (Const 2))) in
  assert_equal (Solver.backtrack [] [cstr1; cstr2]) false

let testBacktrack3 test_ctxt =
  let var1 = Variable.create (Domain.fromList [1;2;3;4;5]) in
  let var2 = Variable.create (Domain.fromList [4;5;7]) in
  let cstr1 = Constraint.BoolConstr (Comparator ((>),
                                             (Var var2),
                                             (Const 6))) in
  let cstr2 = Constraint.BoolConstr (Comparator ((=),
                                             (BinOp ((+),
                                                        (Var var1),
                                                        (Var var2))),
                                             (Const 10))) in
  assert_equal (Solver.backtrack [var1; var2] [cstr1; cstr2]) true;
  assert_equal (Variable.value var1) (Some 3);

  Variable.unassign var2;
  Variable.assign var1 5;
  assert_equal (Solver.backtrack [var1; var2] [cstr1; cstr2]) false

let testPropagate1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 1) and
  var2 = Variable.create (Domain.range 1 2) and
  var3 = Variable.create (Domain.range 1 3) in
  let cstr1 = Constraint.AllDifferent [Var var1; Var var2] and
  cstr2 = Constraint.AllDifferent [Var var2; Var var3] in
  assert_equal (Solver.propagate [var1; var2; var3] [cstr1; cstr2]) false;
  assert_equal (Variable.value var1) (Some 1);
  assert_equal (Variable.value var2) (Some 2);
  assert_equal (Domain.card (Variable.domain var3)) 2

let testSolve test_ctxt =
  let var1 = Variable.create (Domain.range 1 2) and
  var2 = Variable.create (Domain.range 1 2) and
  var3 = Variable.create (Domain.range 2 3) in
  let cstr1 = Constraint.AllDifferent [Var var1; Var var2] and
  cstr2 = Constraint.BoolConstr (Comparator ((<>), (Var var2), (Var var3))) in
  assert_equal (Solver.solve [var1; var2; var3] [cstr1; cstr2]) true;
  assert_equal (Variable.value var1) (Some 1);
  assert_equal (Variable.value var2) (Some 2);
  assert_equal (Variable.value var3) (Some 3)

let suite = [
  "backtrack1">::testBacktrack1;
  "backtrack2">::testBacktrack2;
  "backtrack3">::testBacktrack3;
  "propagate1">::testPropagate1;
  "solve">::testSolve;
]
