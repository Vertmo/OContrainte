(** Domain module : handle finite domains of integers *)

(** Domain type : contains a set of integers *)
type dom

(** An empty domain *)
val empty : dom

(** A domain containing all the integers between start and stop (both included) *)
val range : int -> int -> dom

(** Construct a domain from a list of elements *)
val fromList : int list -> dom

(** Construct a domain from an array of elements *)
val fromArray : int array -> dom

(** Number of elements of the domain *)
val card : dom -> int

(** Get the list of elements contained in the domain *)
val asList : dom -> int list

(** Add an element to a domain *)
val add : dom -> int -> dom

(** Remove an element from a domain *)
val remove : dom -> int -> dom

(** Smallest int in the domain *)
val min : dom -> int option

(** Biggest int in the domain *)
val max : dom -> int option

(** Checks if the domains contains a value *)
val contains : dom -> int -> bool

(** Iter a function on the domain *)
val iter : (int -> unit) -> dom -> unit

(** Checks if the domains contains a value that satisfy a predicate *)
val exists : (int -> bool) -> dom -> bool

(** Keep on the elements verifying a predicate *)
val filter : (int -> bool) -> dom -> dom
