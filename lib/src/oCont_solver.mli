(** Solver module *)

open OCont_variable
open OCont_constraint

(** Solve the problem : returns true if it found a solution, false otherwise. Starts by propagating, then backtracks. *)
val solve : ?varStrat : (var list -> (var option * var list)) -> var list -> constr list -> bool

(** Propagate the constraints : returns true if ended with all variables assigned, false otherwise *)
val propagate : var list -> constr list -> bool

(** Backtrack the problem : returns true if it found a solution, false otherwise *)
val backtrack : var list -> constr list -> bool
