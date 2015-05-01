open Syntax

let slurp file_path =
  let rec iter chan =
    try
      (* interestingly, this doesn't work:
           input_line chan :: iter chan
         I have no ideas what's going wrong *)
      let line = input_line chan in
      line :: iter chan
    with e ->
      close_in_noerr chan;
      []
  in
  String.concat "\n" (iter (open_in file_path))

let parse (str : string) : (exp * int) =
  let rec skip_comment idx =
    match String.get str idx with
    | '\n' -> idx + 1
    | _    -> skip_comment (idx + 1)
  in
  let rec loop idx =
    match String.get str idx with
    | '#' -> loop (skip_comment (idx + 1))
    | '`' ->
        let (e1, idx1) = loop (idx  + 1) in
        let (e2, idx2) = loop idx1 in
        (Backtick (e1, e2), idx2)
    | '.' ->
        let c = String.get str (idx + 1) in
        (Print c, idx + 2)
    | 'r' -> (Print '\n', idx + 1)
    | 'k' -> (K, idx + 1)
    | 's' -> (S, idx + 1)
    | 'i' -> (I, idx + 1)
    | 'v' -> (V, idx + 1)
    | 'c' -> (C, idx + 1)
    | 'd' -> (D, idx + 1)
    | 'e' ->
        let (e, idx1) = loop (idx + 1) in
        (E e, idx1)
    | '@' -> (Read, idx + 1)
    | '?' ->
        let c = String.get str (idx + 1) in
        (Cmp c, idx + 2)
    | '|' -> (Repr, idx + 1)
    | ' ' | '\n' | '\t' -> loop (idx + 1)
    | chr -> raise (Failure ("Invalid char found: " ^ Char.escaped chr))
  in
  loop 0

(* First implementation: A CPS style interpreter. Continuation is used to
 * implement call/cc *)

let rec apply (e1 : exp) (e2 : exp) (cc : char option) (cont : exp -> char option -> exp) =
  match e1 with
  | K -> cont (K1 e2) cc
  | K1 x -> cont x cc
  | S -> cont (S1 e2) cc
  | S1 x -> cont (S2 (x, e2)) cc
  | S2 (x, y) -> eval (Backtick (Backtick (x, e2), Backtick (y, e2))) cc cont
  | I -> cont e2 cc
  | V -> cont V cc
  | C -> cont (Cont cont) cc
  | Cont c -> c e2 cc
  | D -> cont e2 cc
  | D1 f -> eval f cc (fun v1 cc' -> apply v1 e2 cc' cont)
  | Print c -> print_char c; flush stdout; cont e2 cc
  | E x -> x
  | Read ->
      let c = (try Some (input_char stdin)
               with _ -> None) in
      apply e2 (match c with None -> V | Some _ -> I) c cont
  | Cmp c ->
      apply e2 (if Some c = cc then I else V) cc cont
  | Repr ->
      apply e2 (match cc with None -> V | Some c -> Print c) cc cont
  | Backtick (e1, e2) -> raise (Failure "can't apply to backtick")

and eval (exp : exp) (cc : char option) cont =
  match exp with
  | Backtick (arg1, arg2) ->
      eval arg1 cc (fun arg1v cc' ->
        match arg1v with
        | D  -> cont (D1 arg2) cc'
        | v1 -> eval arg2 cc' (fun v2 cc'' -> apply v1 v2 cc'' cont))
  | _ -> cont exp cc

(*
 * This piece of state will be used in staged interpreter (see also
 * documentation for the reason why)
 *
 * Note that it also used in the fallback interpreter of staged interpreter.
 *)

let current_char : (char option) ref  = ref None

(* Interpreter that uses global state, and also continuations are represented as
 * data instead of functions TODO: Talk about this in the report *)

let rec apply_cont (e1 : exp_s) (cont : cont list) : exp_s =
  match cont with
  | [] -> e1
  | DelayGuard e2 :: rest -> begin
      match e1 with
      | D_S -> apply_cont (D1_S e2) rest
      | _   -> eval_ref e2 (ApplyTo e1 :: rest)
    end
  | ApplyTo f :: rest -> apply_ref f e1 rest
  | ApplyDelayed a :: rest -> apply_ref e1 a rest

and apply_ref (e1 : exp_s) (e2 : exp_s) (cont : cont list) : exp_s =
  match e1 with
  | K_S -> apply_cont (K1_S e2) cont
  | K1_S x -> apply_cont x cont
  | S_S -> apply_cont (S1_S e2) cont
  | S1_S x -> apply_cont (S2_S (x, e2)) cont
  | S2_S (x, y) ->
      eval_ref (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cont
  | I_S -> apply_cont e2 cont
  | V_S -> apply_cont V_S cont
  | C_S -> apply_cont (Cont_S cont) cont
  | Cont_S cont' -> apply_cont e2 cont'
  | D_S -> apply_cont e2 cont
  | D1_S f -> eval_ref f (ApplyDelayed e2 :: cont)
  | Print_S c -> print_char c; flush stdout; apply_cont e2 cont
  | E_S x -> x
  | Read_S ->
      let c = try Some (input_char stdin)
              with _ -> None in
      current_char := c;
      apply_ref e2 (match c with None -> V_S | Some _ -> I_S) cont
  | Cmp_S c ->
      apply_ref e2 (if Some c = !current_char then I_S else V_S) cont
  | Repr_S ->
      apply_ref e2 (match !current_char with None -> V_S | Some c -> Print_S c) cont
  | Backtick_S _ ->
      raise (Failure "Can't apply to backtick.")

and eval_ref (exp : exp_s) (cont : cont list) =
  match exp with
  | Backtick_S (arg1, arg2) -> eval_ref arg1 (DelayGuard arg2 :: cont)
  | _ -> apply_cont exp cont

(* First staged interpreter: Very powerful, but loop if programs loop *)

let rec apply_cont_staged (e1 : exp_s) (cont : cont list) : exp_s code =
  match cont with
  | [] -> .< e1 >.
  | DelayGuard e2 :: rest -> begin
      match e1 with
      | D_S -> apply_cont_staged (D1_S e2) rest
      | _   -> eval_staged e2 (ApplyTo e1 :: rest)
    end
  | ApplyTo f :: rest -> apply_staged f e1 rest
  | ApplyDelayed a :: rest -> apply_staged e1 a rest

and apply_staged (e1 : exp_s) (e2 : exp_s) (cont : cont list) : exp_s code =
  match e1 with
  | K_S -> apply_cont_staged (K1_S e2) cont
  | K1_S x -> apply_cont_staged x cont
  | S_S -> apply_cont_staged (S1_S e2) cont
  | S1_S x -> apply_cont_staged (S2_S (x, e2)) cont
  | S2_S (x, y) ->
      eval_staged (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cont
  | I_S -> apply_cont_staged e2 cont
  | V_S -> apply_cont_staged V_S cont
  | C_S -> apply_cont_staged (Cont_S cont) cont
  | Cont_S cont' -> apply_cont_staged e2 cont'
  | D_S -> apply_cont_staged e2 cont
  | D1_S f -> eval_staged f (ApplyDelayed e2 :: cont)
  | Print_S c -> .< let _ = print_char c in .~ (apply_cont_staged e2 cont)  >.
  | E_S x -> .< x >.
  | Read_S ->
      .< let c = try Some (input_char stdin)
                 with _ -> None in
         current_char := c;
         (* TODO: Explain why we can't just use `c` here *)
         apply_ref e2 (match !current_char with None -> V_S | Some _ -> I_S) cont
         (* .~ (apply_staged e2 (match !current_char with None -> V_S | Some _ -> I_S) cont) *)
       >.
  | Cmp_S c ->
      (* TODO: we're being overly conservative here, we can have static values
       * in current_char *)
      .< apply_ref e2 (if Some c = !current_char then I_S else V_S) cont >.
  | Repr_S ->
      (* TODO: Same problem here, we may have statically known value *)
      .< apply_ref e2 (match !current_char with None -> V_S | Some c -> Print_S c) cont >.
  | Backtick_S _ ->
      raise (Failure "Can't apply to backtick.")

and eval_staged (exp : exp_s) (cont : cont list) : exp_s code =
  match exp with
  | Backtick_S (arg1, arg2) -> eval_staged arg1 (DelayGuard arg2 :: cont)
  | _ -> apply_cont_staged exp cont

let _ =
  let contents = slurp Sys.argv.(1) in
  let (p, pos) = parse contents in

  if Array.length Sys.argv == 3 && String.compare Sys.argv.(2) "stage0" == 0 then
    let _ = Print_code.print_code Format.std_formatter .< eval_ref (tr p) [] >. in
    ()
  else if Array.length Sys.argv == 3 && String.compare Sys.argv.(2) "stage" == 0 then
    let _ = Print_code.print_code Format.std_formatter (eval_staged (tr p) []) in
    (* Runcode.(!.) (eval_s1 (tr p) None (fun x _ -> .<x>.)); *)
    ()
  else if Array.length Sys.argv == 3 && String.compare Sys.argv.(2) "cont" == 0 then
    let _ = eval_ref (tr p) [] in
    ()
  else begin
    Printf.printf "last position: %d, string length: %d\n" pos (String.length contents);
    (* FIXME: This assertion fails if program has trailing WS/comments *)
    assert (pos == String.length contents);
    let _ = eval p None (fun x _ -> x) in
    ()
  end
