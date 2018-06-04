(** Variable module : describes an integer variable taking values in a domain *)

(** Variable type : takes integer values *)
type var

(** Create a new variable defined over a specific domain *)
val create : OCont_domain.dom -> var

(** Assign a var to an integer value *)
val assign : var -> int -> unit

(** Unassign a var *)
val unassign : var -> unit

(** Is this variable assigned to a value ? *)
val isAssigned : var -> bool

(** Get the value assigned to this variable *)
val value : var -> int option
