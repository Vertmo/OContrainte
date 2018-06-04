type dom = int list

let empty = []

let rec range start stop =
  if start > stop then invalid_arg "interval";
  if start >= stop
  then []
  else start :: (range (start + 1) stop)

let card d =
  List.length d

let fromList l = List.sort_uniq (-) l

let fromArray a = List.sort_uniq (-) (Array.to_list a)

let asList d = d

let add d n = List.sort_uniq (-) (n::d)

let rec remove d n = match d with
  | [] -> []
  | t::q when t = n -> q
  | t::q -> t::(remove q n)

let min d = match d with
  | [] -> None
  | t::q -> Some t

let rec max d = match d with
  | [] -> None
  | t :: [] -> Some t
  | t :: q -> max q
