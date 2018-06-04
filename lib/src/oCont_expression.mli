(** Expression module : describes expressions of variables *)

open OCont_variable

(** Expression that evaluates to an integer *)
type intExpr = IntConst of int (** integer constant *)
             | Var of var ref (** reference on a variable defined by the user *)
             | IntBinOp of (int -> int -> int) * intExpr * intExpr (** +, *, /, -, ...*)

(** Expression that evaluates to a boolean *)
type boolExpr = BoolConst of bool
              | Comparator of (int -> int -> bool) * intExpr * intExpr (** =, <, >=, ... *)
              | BoolUnOp of (bool -> bool) * boolExpr (** not, ... *)
              | BoolBinOp of (bool -> bool -> bool) * boolExpr * boolExpr (** &&, ||, ... *)


(** All variables in an integer expression *)
val allVarsI : intExpr -> var ref list

(** All variables in a boolean expression *)
val allVarsB : boolExpr -> var ref list

(** Evaluate an integer expression *)
val evalI : intExpr -> int

(** Evaluate a boolean expression *)
val evalB : boolExpr -> bool
