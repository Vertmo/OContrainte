(** Variable module : describes an integer variable taking values in a domain *)

(** Variable type : takes integer values *)
type var

(** Create a new variable defined over a specific domain *)
val create : OCont_domain.dom -> var

(** Get the domain of the variable *)
val domain : var -> OCont_domain.dom

(** Get the cardinality of the domain of the variable *)
val card : var -> int

(** Assign a variable to an integer value *)
val assign : var -> int -> unit

(** Unassign a variable *)
val unassign : var -> unit

(** Is this variable assigned to a value ? *)
val isAssigned : var -> bool

(** Get the value assigned to this variable *)
val value : var -> int option

(** Print a variable *)
val print_var : var -> unit

(** Removes a value from the domain of the variable. If there's only one value left in the domain and the variable is unassigned, assigns the variable to that value. Returns true if the domain was changed, false otherwise *)
val reduceDomain : var -> int -> bool

(** Change the domain of the variable *)
val setDomain : var -> OCont_domain.dom -> unit

(** Copy a variable *)
val copy : var -> var

(** [resetFromCopy v1 v2] sets domain and value of v1 to domain and value of v2 *)
val resetFromCopy : var -> var -> unit
