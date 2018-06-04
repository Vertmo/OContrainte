(** Solver module *)

open OCont_variable
open OCont_constraint

(** Solves the problem : returns true if it found a solution, false otherwise *)
val solve : var list -> constr list -> bool
