type dom = int list

let empty = []

let rec range start stop =
  if start > stop then invalid_arg "interval";
  if start >= stop
  then []
  else start :: (range (start + 1) stop)

let size d =
  List.length d

let from_list l = List.sort_uniq (-) l

let from_array a = List.sort_uniq (-) (Array.to_list a)
