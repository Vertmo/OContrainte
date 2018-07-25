(*                            Mapple flag coloring                                   *)
(* Example taken from : http://opti.recherche.enac.fr/facile/doc/manual004.html#toc7 *)

open Avr

open OContrainte
open OContrainte.Domain
open OContrainte.Operators
open OContrainte.Expression
open OContrainte.Constraint

let adjacent v1 v2 =
  BoolConstr (Comparator ((~<>), (Var v1), (Var v2)))

let gl = PIN22 and rl = PIN24

let () =
  pin_mode gl OUTPUT; digital_write gl LOW;
  pin_mode rl OUTPUT; digital_write rl LOW;

  let red = 0 and white = 1 in
  let dom = fromArray [|red;white|] in
  let l = Variable.create dom and c = Variable.create dom
  and r = Variable.create dom and m = Variable.create dom in
  let vars = [l;c;r;m] in

  let cstrs = [(adjacent c l);
               (adjacent c r);
               (adjacent m c);
               (BoolConstr (Comparator ((~=), (Var m), (Const red))))] in

  if Solver.solve vars cstrs
  then digital_write gl HIGH
  else digital_write rl HIGH
