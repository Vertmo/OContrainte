open OUnit2
open OContrainte
open OContrainte.Operators
open OContrainte.Expression

(* allVars tests *)
let testAllVars1 test_ctxt = assert_equal (allVars (Const 2)) []

let testAllVars2 test_ctxt = assert_equal (allVars (Var (Variable.create (Domain.empty)))) [Variable.create Domain.empty]

let testAllVars3 test_ctxt = assert_equal (allVars (Const true)) []

let testAllVars4 test_ctxt = let expr = Comparator ((~>), (Var (Variable.create (Domain.range 1 3))), (Var (Variable.create (Domain.range 2 4)))) in
  assert_equal (List.length (allVars expr)) 2

let testAllVars5 test_ctxt =
  let var1 = Variable.create (Domain.range 1 3) and
  var2 = Variable.create (Domain.range 1 4) in
  let expr = (Comparator ((~<), ((BinOp ((+), (Var var1), (Var var2)))), (Var var2))) in
  assert_equal (List.length (allVars expr)) 2

(* eval tests *)

let testEval1 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let expr = Var var1 in
  Variable.assign var1 3;
  assert_equal (eval expr) 3;
  Variable.assign var1 1;
  assert_equal (eval expr) 1

let testEvalFailure test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let expr = Var var1 in
  assert_raises (Failure "not all variables are assigned in this expr !") (fun () -> eval expr)

let testEval2 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let expr = BinOp ((+), (Var var1), (Const 3)) in
  Variable.assign var1 3;
  assert_equal (eval expr) 6

let testEval3 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let var2 = Variable.create (Domain.range 0 2) in
  let expr = (Comparator ((~>),
                          (Var var1),
                          (BinOp ((-),
                                     (Var var1),
                           (Var var2))))) in
  Variable.assign var1 3;
  Variable.assign var2 1;
  assert_equal (eval expr) true;
  Variable.assign var2 0;
  assert_equal (eval expr) false

let testEval4 test_ctxt =
  let var1 = Variable.create (Domain.range 1 4) in
  let var2 = Variable.create (Domain.range 4 7) in
  let expr = (BinOp ((&&),
                         (Const true),
                         (Comparator ((~<),
                                      (Var var1),
                                      (Var var2))))) in
  Variable.assign var1 3;
  Variable.assign var2 4;
  assert_equal (eval expr) true

let testEval5 test_ctxt =
  let expr = (MultiOp ((List.fold_left (+) 2),
                          [(Const 3); (Const 4); (Const 5)])) in
  assert_equal (eval expr) 14

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
  "eval5">::testEval5;
]
