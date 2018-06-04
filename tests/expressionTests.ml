open OUnit2
open OContrainte
open OContrainte.Expression

let testAllVars1 test_ctxt = assert_equal (allVarsI (IntConst 2)) []

let testAllVars2 test_ctxt = assert_equal (allVarsI (Var (ref (Variable.create (Domain.empty))))) [ref (Variable.create Domain.empty)]

let testAllVars3 test_ctxt = let expr = IntBinOp ((fun x -> fun y -> x * y),
                                                  (Var (ref (Variable.create (Domain.range 1 5)))),
                                                  (IntBinOp ((+), (Var (ref (Variable.create (Domain.range 1 4)))), (Var (ref (Variable.create (Domain.range 1 4))))))) in
  assert_equal (List.length (allVarsI expr)) 3

let testAllVars4 test_ctxt = assert_equal (allVarsB (BoolConst true)) []

let testAllVars5 test_ctxt = let expr = Comparator ((>), (Var (ref (Variable.create (Domain.range 1 3)))), (Var (ref (Variable.create (Domain.range 2 4))))) in
  assert_equal (List.length (allVarsB expr)) 2

let suite = [
  "allVars1">::testAllVars1;
  "allVars2">::testAllVars2;
  "allVars3">::testAllVars3;
  "allVars4">::testAllVars4;
  "allVars5">::testAllVars5;
]
