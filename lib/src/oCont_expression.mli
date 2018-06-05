(** Expression module : describes expressions of variables *)

open OCont_variable

(** Expression that evaluates to an integer *)
type intExpr = IntConst of int (** integer constant *)
             | Var of var (** reference on a variable defined by the user *)
             | IntUnOp of (int -> int) * intExpr (* -, ... *)
             | IntBinOp of (int -> int -> int) * intExpr * intExpr (** +, *, /, -, ... *)
             | IntMultiOp of (int list -> int) * intExpr list (** reduce (+) 0, ... *)

(** Expression that evaluates to a boolean *)
type boolExpr = BoolConst of bool
              | Comparator of (int -> int -> bool) * intExpr * intExpr (** =, <, >=, ... *)
              | BoolUnOp of (bool -> bool) * boolExpr (** not, ... *)
              | BoolBinOp of (bool -> bool -> bool) * boolExpr * boolExpr (** &&, ||, ... *)


(** All the variables in an integer expression *)
val allVarsI : intExpr -> var list

(** All the variables in a boolean expression *)
val allVarsB : boolExpr -> var list

(** All the variables in this integer expression are assigned *)
val allAssignedI : intExpr -> bool

(** All the variables in this boolean expression are assigned *)
val allAssignedB : boolExpr -> bool

(** Evaluate an integer expression *)
val evalI : intExpr -> int

(** Evaluate a boolean expression *)
val evalB : boolExpr -> bool
