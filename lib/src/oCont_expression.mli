(** Expression module : describes expressions of variables *)

open OCont_variable

(** Expression that evaluates either to a boolean or an integer *)
type 'a expr = Const : 'a -> 'a expr (** constant *)
             | Var : var -> int expr (** reference on a variable defined by the user *)
             | UnOp : ('a -> 'a) * 'a expr -> 'a expr (** (~-), ... *)
             | BinOp : ('a -> 'a -> 'a) * 'a expr * 'a expr -> 'a expr (** +, *, /, -, ... *)
             | MultiOp : ('a list -> 'a) * 'a expr list -> 'a expr (** reduce (+) 0, ... *)
             | Comparator : (int -> int -> bool) * int expr * int expr -> bool expr (** (=), (<=), ... *)

(** All the variables in an expression *)
val allVars : 'a expr -> var list

(** Check if all the variables in this expression are assigned *)
val allAssigned : 'a expr -> bool

(** Evaluate an expression *)
val eval : 'a expr -> 'a
