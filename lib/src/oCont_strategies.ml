let firstVar vars = match vars with
  | [] -> (None, [])
  | t::q -> (Some t, q)

let smallestDomain vars = match vars with
  | [] -> (None, [])
  | _ -> let vMinCard = List.fold_left (fun a v -> if OCont_variable.card a < OCont_variable.card v then a else v) (List.hd vars) vars in
    (Some vMinCard, List.filter (fun v -> not (v == vMinCard)) vars)
