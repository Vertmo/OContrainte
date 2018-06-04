open OUnit2
open OContrainte
open OContrainte.Expression

(* allVars tests *)
let testAllVars1 test_ctxt = assert_equal (allVarsI (IntConst 2)) []

let testAllVars2 test_ctxt = assert_equal (allVarsI (Var (ref (Variable.create (Domain.empty))))) [ref (Variable.create Domain.empty)]

let testAllVars3 test_ctxt = let expr = IntBinOp ((fun x -> fun y -> x * y),
                                                  (Var (ref (Variable.create (Domain.range 1 5)))),
                                                  (IntBinOp ((+), (Var (ref (Variable.create (Domain.range 1 4)))), (Var (ref (Variable.create (Domain.range 1 4))))))) in
  assert_equal (List.length (allVarsI expr)) 3

let testAllVars4 test_ctxt = assert_equal (allVarsB (BoolConst true)) []

let testAllVars5 test_ctxt = let expr = Comparator ((>), (Var (ref (Variable.create (Domain.range 1 3)))), (Var (ref (Variable.create (Domain.range 2 4))))) in
  assert_equal (List.length (allVarsB expr)) 2

(* eval tests *)

let testEval1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let expr = Var (ref var1) in
  Variable.assign var1 3;
  assert_equal (evalI expr) 3;
  Variable.assign var1 1;
  assert_equal (evalI expr) 1

let testEvalFailure test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let expr = Var (ref var1) in
  assert_raises (Failure "not all variables are assigned in this expr !") (fun () -> evalI expr)

let testEval2 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let expr = IntBinOp ((+), (Var (ref var1)), (IntConst 3)) in
  Variable.assign var1 3;
  assert_equal (evalI expr) 6

let testEval3 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let var2 = Variable.create (Domain.range 0 2) in
  let expr = (Comparator ((>),
                          (Var (ref var1)),
                          (IntBinOp ((-),
                                     (Var (ref var1)),
                           (Var (ref var2)))))) in
  Variable.assign var1 3;
  Variable.assign var2 1;
  assert_equal (evalB expr) true;
  Variable.assign var2 0;
  assert_equal (evalB expr) false

let testEval4 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let var2 = Variable.create (Domain.range 4 7) in
  let expr = (BoolBinOp ((&&),
                         (BoolConst true),
                         (Comparator ((<),
                                      (Var (ref var1)),
                                      (Var (ref var2)))))) in
  Variable.assign var1 3;
  Variable.assign var2 4;
  assert_equal (evalB expr) true

let suite = [
  "allVars1">::testAllVars1;
  "allVars2">::testAllVars2;
  "allVars3">::testAllVars3;
  "allVars4">::testAllVars4;
  "allVars5">::testAllVars5;
  "eval1">::testEval1;
  "evalFailure">::testEvalFailure;
  "eval2">::testEval2;
  "eval3">::testEval3;
  "eval4">::testEval4;
]