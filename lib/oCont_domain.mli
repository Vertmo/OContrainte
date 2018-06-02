(** Domain module : handle finite domains of integers *)

(** Domain type : contains a collection of integers *)
type dom

(** An empty domain *)
val empty : dom

(** A domain containing all the integers between start (included) and stop (excluded) *)
val range : int -> int -> dom

(** Number of elements of the domain *)
val size : dom -> int

(** Construct a domain from a list of elements *)
val from_list : int list -> dom

(** Construct a domain from an array of elements *)
val from_array : int array -> dom

(** Add an element to a domain *)
