open OCont_domain

(** Comparison operator *)
type compOp = {
  f: (int -> int -> bool);
  propagate: (dom * dom -> dom * dom);
}

let (~=) = {
  f = (=);
  propagate = fun (d1, d2) ->
    (filter (fun v -> contains d2 v) d1,
     filter (fun v -> contains d1 v) d2);
}

let (~<>) = {
  f = (<>);
  propagate = fun (d1, d2) ->
    ((if (card d2) = 1 then match (min d2) with
         | Some n -> remove d1 n
         | _ -> d1 (* Should not happen *)
       else d1),
     (if (card d1) = 1 then match (min d1) with
         | Some n -> remove d2 n
         | _ -> d2 (* Should not happen *)
      else d2));
}

let (~<) = {
  f = (<);
  propagate = fun (d1, d2) -> match (min d1, max d2) with
    | (Some n1, Some n2) ->
      (filter (fun v -> v < n2) d1,
       filter (fun v -> n1 < v) d2)
    | _ -> (empty, empty)
}

let (~>) = {
  f = (>);
  propagate = fun (d1, d2) -> match (max d1, min d2) with
    | (Some n1, Some n2) ->
      (filter (fun v -> v > n2) d1,
       filter (fun v -> n1 > v) d2)
    | _ -> (empty, empty)
}

let (~<=) = {
  f = (<=);
  propagate = fun (d1, d2) -> match (min d1, max d2) with
    | (Some n1, Some n2) ->
      (filter (fun v -> v <= n2) d1,
       filter (fun v -> n1 <= v) d2)
    | _ -> (empty, empty)
}

let (~>=) = {
  f = (>=);
  propagate = fun (d1, d2) -> match (max d1, min d2) with
    | (Some n1, Some n2) ->
      (filter (fun v -> v >= n2) d1,
       filter (fun v -> n1 >= v) d2)
    | _ -> (empty, empty)
}