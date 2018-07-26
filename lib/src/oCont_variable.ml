open OCont_domain

type var = {
  mutable domain : dom;
  mutable value : int option;
}

let create d = { domain = d; value = None; }

let domain v = v.domain

let card v = card (v.domain)

let assign v n =
  if OCont_domain.contains v.domain n
  then v.value <- Some n
  else invalid_arg "value_not_in_domain"

let unassign v = if OCont_domain.card v.domain > 1 then v.value <- None

let isAssigned v = match v.value with
  | None -> false
  | Some _ -> true

let value v = v.value

let print_var v = match v.value with
  | Some n -> print_int n
  | None -> print_string "?"

let setDomain v d =
  v.domain <- d;
  if (card v = 1) then match min v.domain with
    | Some n -> assign v n
    | None -> ()

let reduceDomain v n =
  let dom2 = remove v.domain n in
  if OCont_domain.card dom2 < OCont_domain.card v.domain
  then (setDomain v dom2; true)
  else false

let copy v = { domain = v.domain; value = v.value }

let resetFromCopy v1 v2 = v1.domain <- v2.domain; v1.value <- v2.value
