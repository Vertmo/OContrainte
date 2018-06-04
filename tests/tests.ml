open OUnit2

let suite =
  "suite">:::[
    test_list DomainTests.suite;
    test_list VariableTests.suite;
    test_list ExpressionTests.suite;
    test_list ConstraintTests.suite;
    test_list SolveTests.suite;
]

let () = run_test_tt_main suite
