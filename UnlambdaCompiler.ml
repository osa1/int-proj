(* A compiler that removes all interpretation costs. *)

open Syntax
open UnlambdaFuns

let rec compile (exp : exp_s) : ('a -> 'a) code =
  match exp with
  | Backtick_S (arg1, arg2) -> .< .~ (compile arg1) .~ (compile arg2) >.
  | K_S -> .< k >.
  | S_S -> .< s >.
  | I_S -> .< i >.
  | V_S -> .< v >.
  | C_S -> .< cc >.
  | D_S -> .< d >.
  | Print_S c -> .< print c >.
  | E_S exp' -> .< e .~ (compile exp') >.
  | Read_S -> .< read >.
  | Cmp_S c -> .< cmp c >.
  | Repr_S -> .< repr >.
  | _ -> assert false
