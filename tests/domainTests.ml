open OUnit2
open OContrainte

let testEmpty test_ctxt = assert_equal (Domain.card (Domain.empty)) 0

let testRange test_ctxt = assert_equal (Domain.card (Domain.range 3 7)) 4

let testAsList test_ctxt = assert_equal (Domain.asList (Domain.range 0 1)) [0]

let testFromList test_ctxt = assert_equal (Domain.asList (Domain.range 3 7)) (Domain.asList (Domain.fromList [5;4;4;6;3;4]))

let testFromArray test_ctxt = assert_equal (Domain.asList (Domain.range 3 7)) (Domain.asList (Domain.fromArray [|4;3;5;6;4|]))

let testAdd1 test_ctxt = assert_equal (Domain.add (Domain.range 3 5) 2) (Domain.fromList [2;3;4])

let testAdd2 test_ctxt = assert_equal (Domain.card (Domain.add (Domain.range 3 6) 4)) 3

let testRemove1 test_ctxt = assert_equal (Domain.remove (Domain.range 3 7) 4) (Domain.fromList [3;5;6])

let testRemove2 test_ctxt = assert_equal (Domain.card (Domain.remove (Domain.range 3 7) 7)) 4

let testMin1 test_ctxt = assert_equal (Domain.min (Domain.fromList [3;6;4;2])) (Some 2)

let testMin2 test_ctxt = assert_equal (Domain.min Domain.empty) None

let testMax1 test_ctxt = assert_equal (Domain.max (Domain.fromList [3;6;4;2])) (Some 6)

let testMax2 test_ctxt = assert_equal (Domain.max Domain.empty) None


let suite = [
  "empty">::testEmpty;
  "range">::testRange;
  "asList">::testAsList;
  "fromList">::testFromList;
  "fromArray">::testFromArray;
  "add1">::testAdd1;
  "add2">::testAdd2;
  "remove1">::testRemove1;
  "remove2">::testRemove2;
  "min1">::testMin1;
  "min2">::testMin2;
  "max1">::testMax1;
  "max2">::testMax2;
]
