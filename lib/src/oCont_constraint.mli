(** Constraint module *)

open OCont_expression

(** Constraint type : either a boolean expression or a predefined constraint *)
type constr = BoolConstr of boolExpr (** is consistent if expression evaluates to true or at least one variable is unassigned *)
            | AllDifferent of OCont_variable.var list (** is consistent if all variables are assigned to different values (or unassigned) *)

(** A constraint is consistent if it's boolean expression evaluates to true or if the predefined condition are met *)
val isConsistent : constr -> bool

(** Check if a list of constraints are all consistent *)
val areConsistent : constr list -> bool
