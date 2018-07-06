(** Operator module *)

open OCont_domain

(** Comparison operator *)
type compOp = {
  f: (int -> int -> bool); (** Corresponding ocaml function *)
  propagate: (dom * dom -> dom * dom); (** Function used in constraint propagation *)
}

(** = operator *)
val (~=) : compOp

(** <> operator *)
val (~<>) : compOp

(** < operator *)
val (~<) : compOp

(** > operator *)
val (~>) : compOp

(** <= operator *)
val (~<=) : compOp

(** >= operator *)
val (~>=) : compOp
