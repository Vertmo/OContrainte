open OCont_domain

type var = {
  mutable domain : dom;
  mutable value : int option;
}

let create d = { domain = d; value = None; }

let domain v = v.domain

let assign v n =
  if OCont_domain.contains v.domain n
  then v.value <- Some n
  else invalid_arg ""

let unassign v = v.value <- None

let isAssigned v = match v.value with
  | None -> false
  | Some _ -> true

let value v = v.value

let print_var v = match v.value with
  | Some n -> ()
  | None -> ()

let setDomain v d =
  v.domain <- d;
  if (card v.domain = 1) then match min v.domain with
    | Some n -> assign v n
    | None -> ()

let setDomain v d =
  v.domain <- d;
  if (card v.domain = 1) then match min v.domain with
    | Some n -> assign v n
    | None -> ()

let reduceDomain v n =
  let dom2 = remove v.domain n in
  if card dom2 < card v.domain
  then (setDomain v dom2; true)
  else false
