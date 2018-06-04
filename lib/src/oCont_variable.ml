type var = {
  domain : OCont_domain.dom;
  mutable value : int option;
}

let create d = { domain = d; value = None; }

let assign v n =
  if List.exists (fun e -> e = n) (OCont_domain.asList v.domain)
  then v.value <- Some n
  else invalid_arg "value_not_in_domain"

let unassign v = v.value <- None

let isAssigned v = match v.value with
  | None -> false
  | Some _ -> true

let value v = v.value
