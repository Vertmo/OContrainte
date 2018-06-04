(** Constraint module *)

open OCont_expression

(** Constraint type : encapsulate a boolean expression expressing the constraint *)
type constr

(** Create a constraint *)
val create : boolExpr -> constr

(** A constraint is consistent if it's boolean expression evaluates to true or at least one of the variable in this expression is not assigned *)
val isConsistent : constr -> bool
