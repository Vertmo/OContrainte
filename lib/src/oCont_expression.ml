open OCont_variable

type intExpr = IntConst of int
             | Var of var ref
             | IntBinOp of (int -> int -> int) * intExpr * intExpr

type boolExpr = BoolConst of bool
              | Comparator of (int -> int -> bool) * intExpr * intExpr
              | BoolUnOp of (bool -> bool) * boolExpr
              | BoolBinOp of (bool -> bool -> bool) * boolExpr * boolExpr

let rec allVarsI e = match e with
  | IntConst _ -> []
  | Var v -> [v]
  | IntBinOp (_, e1, e2) -> (allVarsI e1)@(allVarsI e2)

let rec allVarsB e = match e with
  | BoolConst _ -> []
  | Comparator (_, e1, e2) -> (allVarsI e1)@(allVarsI e2)
  | BoolUnOp (_, e1) -> allVarsB e1
  | BoolBinOp (_, e1, e2) -> (allVarsB e1)@(allVarsB e2)

let rec evalI e = match e with
  | IntConst n -> n
  | IntBinOp (f, e1, e2) -> f (evalI e1) (evalI e2)
  | Var v -> match (value !v) with
    | None -> raise (Failure "not all variables are assigned in this expr !")
    | Some n -> n

let rec evalB e = match e with
  | BoolConst b -> b
  | Comparator (f, e1, e2) -> f (evalI e1) (evalI e2)
  | BoolUnOp (f, e1) -> f (evalB e1)
  | BoolBinOp (f, e1, e2) -> f (evalB e1) (evalB e2)
