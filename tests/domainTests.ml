open OUnit2
open OContrainte;;

let testEmpty test_ctxt = assert_equal (Domain.size (Domain.empty)) 0

let testRange test_ctxt = assert_equal (Domain.size (Domain.range 3 7)) 4

let testFromList test_ctxt = assert_equal (Domain.range 3 7) (Domain.from_list [5;4;4;6;3;4])

let testFromArray test_ctxt = assert_equal (Domain.range 3 7) (Domain.from_array [|4;3;5;6;4|])

let suite =
  ["empty">::testEmpty;
   "range">::testRange;
   "fromList">::testFromList;
   "fromArray">::testFromArray]
