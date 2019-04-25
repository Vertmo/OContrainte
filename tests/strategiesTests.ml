open OUnit2
open OContrainte
open OContrainte.Strategies

let firstVarTest test_ctxt =
  let v1 = Variable.create (Domain.range 1 1) and
  v2 = Variable.create (Domain.range 2 2) and
  v3 = Variable.create (Domain.range 3 3) in
  let (v, vars) = firstVar [v1;v2;v3] in
  assert_equal v (Some v1);
  assert_equal vars [v2;v3]

let smallestDomainTest test_ctxt =
  let v1 = Variable.create (Domain.range 1 5) and
  v2 = Variable.create (Domain.range 2 3) and
  v3 = Variable.create (Domain.range 3 7) in
  let (v, vars) = smallestDomain [v1;v2;v3] in
  assert_equal v (Some v2);
  assert_equal vars [v1;v3]

let suite = [
  "firstVar">::firstVarTest;
  "smallestDomain">::smallestDomainTest;
]
