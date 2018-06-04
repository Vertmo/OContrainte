open OUnit2

let suite =
  "suite">:::[
    test_list DomainTests.suite;
    test_list VariableTests.suite;
    test_list ExpressionTests.suite;
]

let () = run_test_tt_main suite
