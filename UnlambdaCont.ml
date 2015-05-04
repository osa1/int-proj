(*
 * Another Unlambda interpreter that uses a different representation of
 * continuations. TODO: Talk about his in the report.
 *)

open Syntax

let rec apply_cont (e1 : exp_s) (cc : char option) (cont : cont list) : exp_s =
  match cont with
  | [] -> e1
  | DelayGuard e2 :: rest -> begin
      match e1 with
      | D_S -> apply_cont (D1_S e2) cc rest
      | _   -> eval e2 cc (ApplyTo e1 :: rest)
    end
  | ApplyTo f :: rest -> apply f e1 cc rest
  | ApplyDelayed a :: rest -> apply e1 a cc rest

and apply (e1 : exp_s) (e2 : exp_s) (cc : char option) (cont : cont list) : exp_s =
  match e1 with
  | K_S -> apply_cont (K1_S e2) cc cont
  | K1_S x -> apply_cont x cc cont
  | S_S -> apply_cont (S1_S e2) cc cont
  | S1_S x -> apply_cont (S2_S (x, e2)) cc cont
  | S2_S (x, y) ->
      eval (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cc cont
  | I_S -> apply_cont e2 cc cont
  | V_S -> apply_cont V_S cc cont
  | C_S -> apply e2 (Cont_S cont) cc cont
  | Cont_S cont' -> apply_cont e2 cc cont'
  | D_S -> apply_cont e2 cc cont
  | D1_S f -> eval f cc (ApplyDelayed e2 :: cont)
  | Print_S c -> print_char c; flush stdout; apply_cont e2 cc cont
  | E_S x -> x
  | Read_S ->
      let c = try Some (input_char stdin)
              with _ -> None in
      apply e2 (match c with None -> V_S | Some _ -> I_S) c cont
  | Cmp_S c ->
      apply e2 (if Some c = cc then I_S else V_S) cc cont
  | Repr_S ->
      apply e2 (match cc with None -> V_S | Some c -> Print_S c) cc cont
  | Backtick_S _ ->
      raise (Failure "Can't apply to backtick.")

and eval (exp : exp_s) (cc : char option) (cont : cont list) : exp_s =
  match exp with
  | Backtick_S (arg1, arg2) -> eval arg1 cc (DelayGuard arg2 :: cont)
  | _ -> apply_cont exp cc cont
