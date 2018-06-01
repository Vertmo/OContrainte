(** Domain module : handle finite domains of integers *)

(** Domain type : contains a collection of integers *)
type t

(** An empty domain *)
val empty : t

(** A domain containing all the integers between start (included) and stop (excluded) *)
val range : int -> int -> t

(** Number of elements of the domain *)
val size : t -> int

(** Construct a domain from a list of elements *)
val from_list : int list -> t

(** Construct a domain from an array of elements *)
val from_array : int array -> t

(** Add an element to a domain *)
