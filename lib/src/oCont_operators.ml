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
  propagate = fun d -> d;
}

let (~<) = {
  f = (<);
  propagate = fun (d1, d2) ->
    (filter (fun v -> exists (fun e -> v < e) d2) d1,
     filter (fun v -> exists (fun e -> e < v) d1) d2);
}

let (~>) = {
  f = (>);
  propagate = fun (d1, d2) ->
    (filter (fun v -> exists (fun e -> v > e) d2) d1,
     filter (fun v -> exists (fun e -> e > v) d1) d2);
}

let (~<=) = {
  f = (<=);
  propagate = fun (d1, d2) ->
    (filter (fun v -> exists (fun e -> v <= e) d2) d1,
     filter (fun v -> exists (fun e -> e <= v) d1) d2);
}

let (~>=) = {
  f = (>=);
  propagate = fun (d1, d2) ->
    (filter (fun v -> exists (fun e -> v >= e) d2) d1,
     filter (fun v -> exists (fun e -> e >= v) d1) d2);
}
