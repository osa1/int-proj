open QBFSyntax

exception VarNotFound of string

let env0 x = raise (VarNotFound x)
let ext env x v = fun y -> if y = x then v else env y

let rec eval (b : bexp) (env : string -> bool) : bool =
  match b with
  | True -> true
  | False -> false
  | And (b1, b2) -> eval b1 env && eval b2 env
  | Or (b1, b2) -> eval b1 env || eval b2 env
  | Not b1 -> not (eval b1 env)
  | Implies (b1, b2) -> eval (Or (b2, And (Not b2, Not b1))) env
  | Forall (x, b1) ->
      let trywith bv = eval b1 (ext env x bv) in
      trywith true && trywith false
  | Var x -> env x

let rec eval_staged (b : bexp) (env : string -> bool code) : bool code =
  match b with
  | True -> .< true >.
  | False -> .< false >.
  | And (b1, b2) -> .< .~ (eval_staged b1 env) && .~ (eval_staged b2 env) >.
  | Or (b1, b2) -> .< .~ (eval_staged b1 env) || .~ (eval_staged b2 env) >.
  | Not b1 -> .< not .~ (eval_staged b1 env) >.
  | Implies (b1, b2) -> .< .~ (eval_staged (Or (b2, And (Not b2, Not b1))) env) >.
  | Forall (x, b1) ->
      .< let trywith bv = .~ (eval_staged b1 (ext env x .<bv>.)) in
         trywith true && trywith false >.
  | Var x -> env x

let rec optimize (b : bexp) : bexp =
  match b with
  | And (b1, b2) -> begin
      match (optimize b1, optimize b2) with
      | (False, _)
      | (_, False) -> False
      | (True, True) -> True
      | (b1', b2') -> And (b1', b2')
    end
  | Or (b1, b2) -> begin
      match (optimize b1, optimize b2) with
      | (True, _)
      | (_, True) -> True
      | (False, False) -> False
      | (b1', b2') -> Or (b1', b2')
    end
  | Not b' -> begin
      match optimize b' with
      | True -> False
      | False -> True
      | b'' -> Not b''
    end
  | Implies (b1, b2) -> begin
      match optimize b1 with
      | True -> optimize b2
      | False -> True
      | b1' -> Implies (b1', optimize b2)
    end
  | Forall (var, rhs) -> Forall (var, optimize rhs)
  | _ -> b
