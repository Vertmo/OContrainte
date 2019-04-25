open OCont_variable

(** Choose first variable of the list *)
val firstVar : var list -> (var option * var list)

(** Take the variable with the smallest domain *)
val smallestDomain : var list -> (var option * var list)
