type dom = Range of int * int | Arr of int array

let empty = Arr [||]

let rec range start stop =
  if start > stop then invalid_arg "range";
  Range (start, stop)

let fromList l = Arr (Array.of_list (List.sort_uniq (-) l))

let fromArray a = Arr (Array.of_list (List.sort_uniq (-) (Array.to_list a)))

let asList d = match d with
  | Range (start,stop) -> let rec aux k n acc =
                            if k > n then acc else aux k (n-1) (n :: acc)
    in aux start (stop-1) []
  | Arr a -> Array.to_list a

let card d = match d with
  | Range (start, stop) -> stop - start
  | Arr a -> Array.length a

let add d n = match d with
  | Range (start, stop) when n >= start && n < stop -> d
  | Range (start, stop) when n < start -> Arr (Array.of_list (n::(asList d)))
  | Range (start, stop) -> Arr (Array.of_list ((asList d)@[n]))
  | Arr a -> Arr (Array.of_list (List.sort_uniq (-) (n::(Array.to_list a))))

let rec remove d n = match d with
  | Range (start, stop) when n < start || n >= stop -> d
  | Range (start, stop) -> Arr (Array.of_list ((asList (Range (start,n)))@(asList (Range (n+1,stop)))))
  | Arr a -> Arr (Array.of_list (List.filter (fun e -> e <> n) (Array.to_list a)))

let min d = match d with
  | Range (start, stop) -> Some start
  | Arr [||] -> None
  | Arr a -> Some a.(0)

let rec max d = match d with
  | Range (start, stop) -> Some stop
  | Arr [||] -> None
  | Arr a -> Some a.(Array.length a - 1)

let contains d n = match d with
  | Range (start, stop) -> n >= start && n < stop
  | Arr a -> Array.exists (fun e -> e = n) a

let iter f d = match d with
  | Range (start, stop) -> let i = ref start in
    while !i < stop do f !i; i := !i + 1 done
  | Arr a -> Array.iter f a

let exists p d = match d with
  | Range (start, stop) -> let found = ref false and i = ref start in
    while !i < stop && not !found do found := p !i; i := !i + 1 done;
    !found
  | Arr a -> Array.exists p a
