open OUnit2

open OContrainte.Domain
open OContrainte.Operators

let testPropagateEq test_ctxt =
  let dom1 = (range 1 7) and dom2 = fromList [0;1;2] in
  let (dom1, dom2) = (~=).propagate (dom1, dom2) in
  assert_equal (asList dom1) (asList dom2);
  assert_equal (asList dom1) [1;2]


let testPropagateSupEq test_ctxt =
  let dom1 = (range 1 7) and dom2 = fromList [3;4;9] in
  let (dom1, dom2) = (~>=).propagate (dom1, dom2) in
  assert_equal (asList dom1) [3;4;5;6];
  assert_equal (asList dom2) [3;4]

let testPropagateInf test_ctxt =
  let dom1 = (range 1 7) and dom2 = fromList [0;3;4] in
  let (dom1, dom2) = (~<).propagate (dom1, dom2) in
  assert_equal (asList dom1) [1;2;3];
  assert_equal (asList dom2) [3;4]

let suite = [
  "propagateEq">::testPropagateEq;
  "propagateSupEq">::testPropagateSupEq;
  "propagateInf">::testPropagateInf;
]
