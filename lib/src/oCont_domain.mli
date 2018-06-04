(** Domain module : handle finite domains of integers *)

(** Domain type : contains a set of integers *)
type dom

(** An empty domain *)
val empty : dom

(** A domain containing all the integers between start (included) and stop (excluded) *)
val range : int -> int -> dom

(** Number of elements of the domain *)
val card : dom -> int

(** Construct a domain from a list of elements *)
val fromList : int list -> dom

(** Construct a domain from an array of elements *)
val fromArray : int array -> dom

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