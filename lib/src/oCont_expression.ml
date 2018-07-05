open OCont_variable

type 'a expr = Const : 'a -> 'a expr (** constant *)
             | Var : var -> int expr (** reference on a variable defined by the user *)
             | UnOp : ('a -> 'a) * 'a expr -> 'a expr (* -, ... *)
             | BinOp : ('a -> 'a -> 'a) * 'a expr * 'a expr -> 'a expr (** +, *, /, -, ... *)
             | MultiOp : ('a list -> 'a) * 'a expr list -> 'a expr (** reduce (+) 0, ... *)
             | Comparator : (int -> int -> bool) * int expr * int expr -> bool expr (** (=), (<=), ... *)

let removeDuplicates l = List.fold_left (fun xs x -> if List.memq x xs then xs else x::xs) [] l

let rec allVars : type a. a expr -> var list = function e -> match e with
  | Const _ -> []
  | Var v -> [v]
  | UnOp (_, e1) -> allVars e1
  | BinOp (_, e1, e2) -> removeDuplicates ((allVars e1)@(allVars e2))
  | MultiOp (_, es) -> removeDuplicates (List.flatten (List.map allVars es))
  | Comparator (_, e1, e2) -> removeDuplicates ((allVars e1)@(allVars e2))

let rec allAssigned : type a. a expr -> bool = function e -> match e with
  | Const _ -> true
  | Var v when value v = None -> false
  | Var v -> true
  | UnOp (_, e1) -> allAssigned e1
  | BinOp (_, e1, e2) -> allAssigned e1 && allAssigned e2
  | MultiOp (_, es) -> List.fold_left (fun a e -> a && allAssigned e) true es
  | Comparator (_, e1, e2) -> allAssigned e1 && allAssigned e2

let rec eval : type a. a expr -> a = function e -> match e with
  | Const n -> n
  | UnOp (f, e1) -> f (eval e1)
  | BinOp (f, e1, e2) -> f (eval e1) (eval e2)
  | MultiOp (f, es) -> f (List.map eval es)
  | Comparator (f, e1, e2) -> f (eval e1) (eval e2)
  | Var v -> match (value v) with
    | None -> raise (Failure "not all variables are assigned in this expr !")
    | Some n -> n
