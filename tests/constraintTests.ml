open OUnit2
open OContrainte
open OContrainte.Expression

let testConsistent1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.create (Comparator ((<),
                                            (Var (ref var1)),
                                            (Var (ref var2)))) in
  Variable.assign var1 3;
  Variable.assign var2 6;
  assert_equal (Constraint.isConsistent cstr) true

let testConsistent2 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.create (Comparator ((<),
                                            (Var (ref var1)),
                                            (Var (ref var2)))) in
  Variable.assign var1 5;
  Variable.assign var2 4;
  assert_equal (Constraint.isConsistent cstr) false

let testConsistent3 test_ctxt =
  let var1 = Variable.create (Domain.range 1 6) in
  let var2 = Variable.create (Domain.range 4 8) in
  let cstr = Constraint.create (Comparator ((<),
                                            (Var (ref var1)),
                                            (Var (ref var2)))) in
  Variable.assign var1 5;
  assert_equal (Constraint.isConsistent cstr) true

let suite = [
  "consistent1">::testConsistent1;
  "consistent2">::testConsistent2;
  "consistent3">::testConsistent3;
]
