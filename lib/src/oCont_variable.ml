type var = {
  domain : OCont_domain.dom;
  mutable value : int option;
}

let create d = { domain = d; value = None; }

let domain v = v.domain

let assign v n =
  if OCont_domain.contains v.domain n
  then v.value <- Some n
  else invalid_arg "value_not_in_domain"

let unassign v = v.value <- None

let isAssigned v = match v.value with
  | None -> false
  | Some _ -> true

let value v = v.value

let print_var v = match v.value with
  | Some n -> ()
  | None -> ()
