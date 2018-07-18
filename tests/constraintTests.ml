open OUnit2
open OContrainte
open OContrainte.Operators
open OContrainte.Expression

let testBoolConstr1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.BoolConstr (Comparator ((~<),
                                            (Var var1),
                                            (Var var2))) in
  Variable.assign var1 3;
  Variable.assign var2 6;
  assert_equal (Constraint.isConsistent cstr) true

let testBoolConstr2 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.BoolConstr (Comparator ((~<),
                                            (Var var1),
                                            (Var var2))) in
  Variable.assign var1 5;
  Variable.assign var2 4;
  assert_equal (Constraint.isConsistent cstr) false

let testBoolConstr3 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.BoolConstr (Comparator ((~<),
                                            (Var var1),
                                            (Var var2))) in
  Variable.assign var1 5;
  assert_equal (Constraint.isConsistent cstr) true

let testAllDifferent test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) and
  var2 = Variable.create (Domain.range 1 6) and
  var3 = Variable.create (Domain.range 1 6) in
  let cstr = Constraint.AllDifferent [Var var1; Var var2; Var var3] in
  assert_equal (Constraint.isConsistent cstr) true;
  Variable.assign var1 3;
  Variable.assign var2 2;
  assert_equal (Constraint.isConsistent cstr) true;
  Variable.assign var3 3;
  assert_equal (Constraint.isConsistent cstr) false

let testPropagate1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 5) and
  var2 = Variable.create (Domain.range 1 5) and
  var3 = Variable.create (Domain.range 1 3) in
  let cstr = Constraint.AllDifferent [Var var1; Var var2; Var var3] in
  Variable.assign var1 3;
  assert_equal (Constraint.propagate cstr) true;
  assert_equal (Constraint.propagate cstr) false;
  assert_equal (Domain.card (Variable.domain var2)) 4;
  Variable.assign var2 1;
  assert_equal (Constraint.propagate cstr) true;
  assert_equal (Variable.value var3) (Some 2)

let testPropagate2 test_ctxt =
  let var1 = Variable.create (Domain.range 1 5) in
  let cstr = Constraint.BoolConstr (Comparator ((~>), (Var var1), (Const 3))) in
  assert_equal (Constraint.propagate cstr) true;
  assert_equal (Constraint.propagate cstr) false;
  assert_equal (Variable.value var1) None;
  assert_equal (Domain.card (Variable.domain var1)) 2

let testPropagate3 test_ctxt =
  let var1 = Variable.create (Domain.range 1 5) and
  var2 = Variable.create (Domain.range 2 3) in
  let cstr = Constraint.BoolConstr (Comparator ((~<=), (Var var1), (Var var2))) in
  assert_equal (Constraint.propagate cstr) true;
  assert_equal (Constraint.propagate cstr) false;
  assert_equal (Domain.card (Variable.domain var1)) 3

let suite = [
  "boolConstr1">::testBoolConstr1;
  "boolConstr2">::testBoolConstr2;
  "boolConstr3">::testBoolConstr3;
  "allDifferent">::testAllDifferent;
  "propagate1">::testPropagate1;
  "propagate2">::testPropagate2;
  "propagate3">::testPropagate3;
]
