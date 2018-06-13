open OUnit2
open OContrainte
open OContrainte.Expression

let testBoolConstr1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.BoolConstr (Comparator ((<),
                                            (Var var1),
                                            (Var var2))) in
  Variable.assign var1 3;
  Variable.assign var2 6;
  assert_equal (Constraint.isConsistent cstr) true

let testBoolConstr2 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.BoolConstr (Comparator ((<),
                                            (Var var1),
                                            (Var var2))) in
  Variable.assign var1 5;
  Variable.assign var2 4;
  assert_equal (Constraint.isConsistent cstr) false

let testBoolConstr3 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.BoolConstr (Comparator ((<),
                                            (Var var1),
                                            (Var var2))) in
  Variable.assign var1 5;
  assert_equal (Constraint.isConsistent cstr) true

let testAllDifferent test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) and
  var2 = Variable.create (Domain.range 1 6) and
  var3 = Variable.create (Domain.range 1 6) in
  let cstr = Constraint.AllDifferent [var1; var2; var3] in
  assert_equal (Constraint.isConsistent cstr) true;
  Variable.assign var1 3;
  Variable.assign var2 2;
  assert_equal (Constraint.isConsistent cstr) true;
  Variable.assign var3 3;
  assert_equal (Constraint.isConsistent cstr) false

let suite = [
  "boolConstr1">::testBoolConstr1;
  "boolConstr2">::testBoolConstr2;
  "boolConstr3">::testBoolConstr3;
  "allDifferent">::testAllDifferent;
]
