open OUnit2
open DomainTests

let suite =
  "suite">:::
  [test_list DomainTests.suite]

let () = run_test_tt_main suite
