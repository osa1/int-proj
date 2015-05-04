(* Put in a separate module because of MetaOCaml's restriction of having
 * declarations in different compilation unit *)

open Syntax

(* NOTE: It seems like MetaOCaml is able to lift constructors without any
 * arguments(although it uses Obj.magic for that), so maybe some of the
 * patterns may be replaced with a wildcard here *)

(* NOTE: Even after all those boilerplate, there are still some problems with
 * MetaOCaml printing. Let's say I have these modules:
 *
 * A: Defines `eval_ref`.
 *    Defines `gen_code` that generates code that references `eval_ref`.
 * B: Calls `A.gen_code` and prints code object.
 *
 * The printed code will have `(* CSP eval_ref *)` instead of `A.eval_ref`.
 *)

let rec lift_exp_s (exp : exp_s) : exp_s code =
  match exp with
  | Backtick_S (e1, e2) -> .< Backtick_S ( .~ (lift_exp_s e1), .~ (lift_exp_s e2) ) >.
  | K_S -> .< K_S >.
  | K1_S e -> .< K1_S .~ (lift_exp_s e) >.
  | S_S -> .< S_S >.
  | S1_S e -> .< S1_S .~ (lift_exp_s e) >.
  | S2_S (e1, e2) -> .< S2_S ( .~ (lift_exp_s e1), .~ (lift_exp_s e2) ) >.
  | I_S -> .< I_S >.
  | V_S -> .< V_S >.
  | C_S -> .< C_S >.
  | Cont_S conts -> .< Cont_S .~ (lift_conts conts) >.
  | D_S -> .< D_S >.
  | D1_S e -> .< D1_S .~ (lift_exp_s e) >.
  | Print_S c -> .< Print_S c >. (* char is lifted automatically *)
  | E_S e -> .< E_S .~ (lift_exp_s e) >.
  | Read_S -> .< Read_S >.
  | Cmp_S c -> .< Cmp_S c >.
  | Repr_S -> .< Repr_S >.

and lift_conts (conts : cont list) : (cont list) code =
  match conts with
  | [] -> .< [] >.
  | c :: cs -> .< .~ (lift_cont c) :: .~ (lift_conts cs) >.

and lift_cont (cont : cont) : cont code =
  match cont with
  | DelayGuard e -> .< DelayGuard .~ (lift_exp_s e) >.
  | ApplyTo e -> .< ApplyTo .~ (lift_exp_s e) >.
  | ApplyDelayed e -> .< ApplyDelayed .~ (lift_exp_s e) >.
