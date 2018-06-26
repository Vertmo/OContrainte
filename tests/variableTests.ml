open OUnit2
open OContrainte;;

let testAssign test_ctxt =
  let v = Variable.create (Domain.range 3 7) in
  assert_equal (Variable.value v) None;
  assert_equal (Variable.isAssigned v) false;
  Variable.assign v 4;
  assert_equal (Variable.value v) (Some 4);
  assert_equal (Variable.isAssigned v) true

let testUnassign test_ctxt =
  let v = Variable.create (Domain.range 3 7) in
  Variable.assign v 6;
  assert_equal (Variable.value v) (Some 6);
  assert_equal (Variable.isAssigned v) true;
  Variable.unassign v;
  assert_equal (Variable.value v) None;
  assert_equal (Variable.isAssigned v) false

let testBadValue test_ctxt =
  let v = Variable.create (Domain.range 3 7) in
  assert_raises (Invalid_argument "") (fun () -> Variable.assign v 8)

let suite = [
  "assign">::testAssign;
  "unAssign">::testUnassign;
  "badValue">::testBadValue;
]
