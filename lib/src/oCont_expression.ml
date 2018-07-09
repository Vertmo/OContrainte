open OCont_variable
open OCont_domain
open OCont_operators

type 'a expr = Const : 'a -> 'a expr
             | Var : var -> int expr
             | UnOp : ('a -> 'a) * 'a expr -> 'a expr
             | BinOp : ('a -> 'a -> 'a) * 'a expr * 'a expr -> 'a expr
             | MultiOp : ('a list -> 'a) * 'a expr list -> 'a expr
             | Comparator : compOp * int expr * int expr -> bool expr

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
  | Comparator (c, e1, e2) -> c.f (eval e1) (eval e2)
  | Var v -> match (value v) with
    | None -> raise (Failure "not all variables are assigned in this expr !")
    | Some n -> n

let propagate : type a. a expr -> bool = function e -> match e with
  | Comparator (c, e1, e2) -> (match (e1, e2) with
      | ((Var v1), (Var v2)) ->
        let (dom1, dom2) = c.propagate (domain v1, domain v2) in
        let changed = (card dom1) < (card (domain v1)) || (card dom2) < (card (domain v2)) in
        setDomain v1 dom1; setDomain v2 dom2;
        changed
      | ((Var v), _) when allAssigned e2 ->
        let (dom, _) = c.propagate (domain v, fromList [eval e2]) in
        let changed = (card dom) < (card (domain v)) in
        setDomain v dom; changed
      | (_, (Var v)) when allAssigned e1 ->
        let (dom, _) = c.propagate (domain v, fromList [eval e1]) in
        let changed = (card dom) < (card (domain v)) in
        setDomain v dom; changed
      | _ -> false)
  | _ -> false
