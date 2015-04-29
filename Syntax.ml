(* syntax tree for basic interpreter *)
type exp =
  | Backtick   of exp * exp
  | K
  | K1         of exp
  | S
  | S1         of exp
  | S2         of exp * exp
  | I
  | V
  | C
  | Cont       of (exp -> char option -> exp)
  | D
  | D1         of exp
  | Print      of char
  (* Unlambda 2 extensions *)
  | E          of exp
  | Read
  | Cmp        of char
  | Repr

(* syntax tree for staged interpreter, only difference is continuations *)
type exp_s =
  | Backtick_S of exp_s * exp_s
  | K_S
  | K1_S       of exp_s
  | S_S
  | S1_S       of exp_s
  | S2_S       of exp_s * exp_s
  | I_S
  | V_S
  | C_S
  | Cont_S     of (exp_s -> char option -> exp_s code)
  | D_S
  | D1_S       of exp_s
  | Print_S    of char
  (* Unlambda 2 extensions *)
  | E_S        of exp_s
  | Read_S
  | Cmp_S      of char
  | Repr_S

let rec tr (exp : exp) : exp_s =
  match exp with
  | Backtick (e1, e2) -> Backtick_S (tr e1, tr e2)
  | K -> K_S
  | K1 e -> K1_S (tr e)
  | S -> S_S
  | S1 e -> S1_S (tr e)
  | S2 (e1, e2) -> S2_S (tr e1, tr e2)
  | I -> I_S
  | V -> V_S
  | C -> C_S
  | D -> D_S
  | D1 e -> D1_S (tr e)
  | Print c -> Print_S c
  | E e -> E_S (tr e)
  | Read -> Read_S
  | Cmp c -> Cmp_S c
  | Repr -> Repr_S
  | _ -> assert false
