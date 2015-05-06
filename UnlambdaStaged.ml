(* Staged interpreter *)

(*
 * NOTE: I did the mistake of evaluating only the subterms in some cases. The
 * problem was that evaluating a subterm may update "current character" state,
 * so combining the results is tricky and leads to more complex staged
 * interpreter functions.  I think one possible nice way to solve this is to
 * use 1) a local(maybe module-level) state or state monad(which is not nice to
 * use on OCaml) 2) use "static-dynamic" values as described in
 * "Supercompilation by Staging" by Jun Inoue.
 * (http://meta2014.pereslavl.ru/papers/2014_Inoue__Supercompiling_with_Staging.pdf)
 *
 * It's fixed now, but we lost some opportunities for optimizations.
 *)

open CmdArgs
open Lift
open Syntax

let rec apply_cont (e1 : exp_s) (cc : char option) (cont : cont list) : exp_s code =
  (*Printf.printf "current term: %s, cont stack depth: %d\n"
      (exp_to_string @@ trb e1) (List.length cont);*)
  (* Printf.printf "applying %s to %s\n" (exp_to_string @@ trb e1) (cont_list_to_string cont); *)
  match cont with
  | [] -> .< .~ (lift_exp_s e1) >.
  | DelayGuard e2 :: rest -> begin
      match e1 with
      | D_S -> apply_cont (D1_S e2) cc rest
      | _   -> eval e2 cc (ApplyTo e1 :: rest)
    end
  | ApplyTo f :: rest -> apply f e1 cc rest
  | ApplyDelayed a :: rest -> apply e1 a cc rest

and apply (e1 : exp_s) (e2 : exp_s) (cc : char option) (cont : cont list) : exp_s code =
  match e1 with
  | K_S -> apply_cont (K1_S e2) cc cont
  | K1_S x -> apply_cont x cc cont
  | S_S -> apply_cont (S1_S e2) cc cont
  | S1_S x -> apply_cont (S2_S (x, e2)) cc cont
  | S2_S (x, y) ->
      if opts.eval_S then
        eval (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cc cont
      else
        .< UnlambdaCont.eval (Backtick_S
                               (Backtick_S ( .~ (lift_exp_s x), .~ (lift_exp_s e2)),
                                Backtick_S ( .~ (lift_exp_s y), .~ (lift_exp_s e2))))
                             .~ (lift_cc cc) .~ (lift_conts cont) >.
  | I_S -> apply_cont e2 cc cont
  | V_S -> apply_cont V_S cc cont
  | C_S -> apply e2 (Cont_S cont) cc cont
  | Cont_S cont' ->
      if opts.eval_cc then
        apply_cont e2 cc cont'
      else
        .< UnlambdaCont.apply_cont .~ (lift_exp_s e2) .~ (lift_cc cc) .~ (lift_conts cont') >.
  | D_S -> apply_cont e2 cc cont
  | D1_S f -> eval f cc (ApplyDelayed e2 :: cont)
  | Print_S c -> .< let _ = print_char c in .~ (apply_cont e2 cc cont) >.
  | E_S x -> .< .~ (lift_exp_s x) >.
  | Read_S ->
      .< let c = try Some (input_char stdin)
                 with _ -> None in
         match c with
         | None   ->
            .~ (if opts.eval_eof then
                  apply e2 V_S None cont
                else
                  .< UnlambdaCont.apply .~ (lift_exp_s e2) V_S None .~ (lift_conts cont) >.)
         | Some _ ->
             (* TODO: Briefly talk about why we can't use concrete character
              * here in staged computation to specialize further *)
             UnlambdaCont.apply .~ (lift_exp_s e2) I_S c .~ (lift_conts cont)
       >.
  | Cmp_S c -> apply e2 (if Some c = cc then I_S else V_S) cc cont
  | Repr_S -> apply e2 (match cc with None -> V_S | Some c -> Print_S c) cc cont
  | Backtick_S _ ->
      raise (Failure "Can't apply to backtick.")

and eval (exp : exp_s) (cc : char option) (cont : cont list) : exp_s code =
  match exp with
  | Backtick_S (arg1, arg2) -> eval arg1 cc (DelayGuard arg2 :: cont)
  | _ -> apply_cont exp cc cont
