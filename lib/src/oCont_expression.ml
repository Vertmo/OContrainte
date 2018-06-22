open OCont_variable

type intExpr = IntConst of int
             | Var of var
             | IntUnOp of (int -> int) * intExpr
             | IntBinOp of (int -> int -> int) * intExpr * intExpr
             | IntMultiOp of (int list -> int) * intExpr list

type boolExpr = BoolConst of bool
              | Comparator of (int -> int -> bool) * intExpr * intExpr
              | BoolUnOp of (bool -> bool) * boolExpr
              | BoolBinOp of (bool -> bool -> bool) * boolExpr * boolExpr

let removeDuplicates l = List.fold_left (fun xs x -> if List.mem x xs then xs else x::xs) [] l

let rec allVarsI e = match e with
  | IntConst _ -> []
  | Var v -> [v]
  | IntUnOp (_, e1) -> allVarsI e1
  | IntBinOp (_, e1, e2) -> removeDuplicates ((allVarsI e1)@(allVarsI e2))
  | IntMultiOp (_, es) -> removeDuplicates (List.flatten (List.map allVarsI es))

let rec allVarsB e = match e with
  | BoolConst _ -> []
  | Comparator (_, e1, e2) -> removeDuplicates ((allVarsI e1)@(allVarsI e2))
  | BoolUnOp (_, e1) -> allVarsB e1
  | BoolBinOp (_, e1, e2) -> removeDuplicates ((allVarsB e1)@(allVarsB e2))

let rec allAssignedI e = match e with
  | IntConst _ -> true
  | Var v when value v = None -> false
  | Var v -> true
  | IntUnOp (_, e1) -> allAssignedI e1
  | IntBinOp (_, e1, e2) -> allAssignedI e1 && allAssignedI e2
  | IntMultiOp (_, es) -> List.fold_left (fun a -> fun e -> a && allAssignedI e) true es

let rec allAssignedB e = match e with
  | BoolConst _ -> true
  | Comparator (_, e1, e2) -> allAssignedI e1 && allAssignedI e2
  | BoolUnOp (_, e1) -> allAssignedB e1
  | BoolBinOp (_, e1, e2) -> allAssignedB e1 && allAssignedB e2

let rec evalI e = match e with
  | IntConst n -> n
  | IntUnOp (f, e1) -> f (evalI e1)
  | IntBinOp (f, e1, e2) -> f (evalI e1) (evalI e2)
  | IntMultiOp (f, es) -> f (List.map evalI es)
  | Var v -> match (value v) with
    | None -> raise (Failure "not all variables are assigned in this expr !")
    | Some n -> n

let rec evalB e = match e with
  | BoolConst b -> b
  | Comparator (f, e1, e2) -> f (evalI e1) (evalI e2)
  | BoolUnOp (f, e1) -> f (evalB e1)
  | BoolBinOp (f, e1, e2) -> f (evalB e1) (evalB e2)
