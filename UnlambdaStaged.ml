(* Staged interpreter *)

open Syntax
open Lift

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
  | ApplyTo f :: rest -> apply_staged f e1 cc rest
  | ApplyDelayed a :: rest -> apply_staged e1 a cc rest

and apply_staged (e1 : exp_s) (e2 : exp_s) (cc : char option) (cont : cont list) : exp_s code =
  match e1 with
  | K_S -> apply_cont (K1_S e2) cc cont
  | K1_S x -> apply_cont x cc cont
  | S_S -> apply_cont (S1_S e2) cc cont
  | S1_S x -> apply_cont (S2_S (x, e2)) cc cont
  | S2_S (x, y) ->
      (* .< UnlambdaCont.eval (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cont >. *)
      (*
       *)
      let app_1 = eval (Backtick_S (x, e2)) cc [] in
      let app_2 = eval (Backtick_S (y, e2)) cc [] in
      .< UnlambdaCont.eval (Backtick_S (.~app_1, .~app_2)) cc .~ (lift_conts cont) >.
      (* eval (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cc cont *)
  | I_S -> apply_cont e2 cc cont
  | V_S -> apply_cont V_S cc cont
  | C_S -> apply_staged e2 (Cont_S cont) cc cont
  | Cont_S cont' ->
      (* apply_cont e2 cont'
       * We're using interpreter here to break the loop *)
      (* .< apply_cont .~ (lift_exp_s e2) .~ (lift_conts cont') >. *)
      apply_cont e2 cc cont'
  | D_S -> apply_cont e2 cc cont
  | D1_S f -> eval f cc (ApplyDelayed e2 :: cont)
  | Print_S c -> .< let _ = print_char c in .~ (apply_cont e2 cc cont) >.
  | E_S x -> .< .~ (lift_exp_s x) >.
  | Read_S ->
      .< let c = try Some (input_char stdin)
                 with _ -> None in
         match c with
         | None   ->
             (* UnlambdaCont.apply e2 V_S c .~ (lift_conts cont) *)
             (* TODO: Explain the optimization here *)
             .~ (apply_staged e2 V_S None cont)

         | Some _ -> UnlambdaCont.apply e2 I_S c .~ (lift_conts cont)
       >.
  | Cmp_S c ->
      (* TODO: we're being overly conservative here, we can have static values
       * in current_char *)
      .< UnlambdaCont.apply e2 (if Some c = cc then I_S else V_S) cc
                            .~ (lift_conts cont) >.
  | Repr_S ->
      (* TODO: Same problem here, we may have statically known value *)
      .< UnlambdaCont.apply e2 (match cc with None -> V_S | Some c -> Print_S c) cc
                            .~ (lift_conts cont) >.
  | Backtick_S _ ->
      raise (Failure "Can't apply to backtick.")

and eval (exp : exp_s) (cc : char option) (cont : cont list) : exp_s code =
  match exp with
  | Backtick_S (arg1, arg2) -> eval arg1 cc (DelayGuard arg2 :: cont)
  | _ -> apply_cont exp cc cont
