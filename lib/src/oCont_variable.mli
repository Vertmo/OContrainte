(** Variable module : describes an integer variable taking values in a domain *)

(** Variable type : takes integer values *)
type var

(** Create a new variable defined over a specific domain *)
val create : OCont_domain.dom -> var
