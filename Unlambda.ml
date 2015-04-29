open Syntax

let current_char : (char option) ref  = ref None

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

(* First staged interpreter: Very powerful, but loop if programs loop *)

and apply_s1 (e1 : exp_s) (e2 : exp_s) (cc : char option)
             (cont : exp_s -> char option -> exp_s code) : exp_s code =
  match e1 with
  | K_S -> cont (K1_S e2) cc
  | K1_S x -> cont x cc
  | S_S -> cont (S1_S e2) cc
  | S1_S x -> cont (S2_S (x, e2)) cc
  | S2_S (x, y) -> eval_s1 (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cc cont
  | I_S -> cont e2 cc
  | V_S -> cont V_S cc
  | D_S -> cont e2 cc
  | C_S -> cont (Cont_S cont) cc
  | Cont_S c -> c e2 cc
  | D1_S f -> eval_s1 f cc (fun v1 cc' -> apply_s1 v1 e2 cc' cont)
  | Print_S c ->
      (* ops.. apparently seq syntax is not supported in quotations
       * .< print_char c; .~ (cont e2 cc) >. *)
      .< let _ = print_char c in .~ (cont e2 cc) >.
  | E_S x -> .< x >.
  | Read_S ->
      (* I'm afraid it's not possible to implement this without using a referecence cell *)

      (* this is very nice, MetaOCaml gives an error, saying that we can't use
       * c in next splicing because basically it's dynamic and can't be known
       * until runtime *)
      (* .< let c = (try Some (input_char stdin)
                     with _ -> None) in
            .~ (apply_s1 e2 (match c with None -> V | Some _ -> I) c cont) >. *)

      (* TODO: find a way to make this working *)
      .< let c = (try Some (input_char stdin)
                  with _ -> None) in

         match c with
         | None    -> .~ (apply_s1 e2 V_S None cont)
         | Some c1 ->
             (* Since c1 is dynamic enough(e.g. there are too many branches if
              * we want to specialize depending on all possible values of it)
              * we want to fallback to interpreter at this point *)

             assert false (* .~ (apply_s1 e2 I_S (Some c1) cont) *)
       >.
  | Cmp_S c ->
      (* here let's say cc is Dynamic, generated code should include a `match _
       * with` expression, but that will be the scr part? *)

      (* Current character need to be passed up to this point somehow, but we
       * don't have tail-calls to pass some state to next steps of the
       * interpreter -- the whole point is to remove those and have an OCaml
       * program that does what our interpreted program does. So what we need
       * to do is to add a top-level state(reference cell) for current
       * character, and use it herer
       *
       * But then there's one more tricky thing... We want to fallback to
       * interpreter when at some points(for example, when we're having a
       * dynamic input and we need to branch depending on that). To be able to
       * do that interpreter need to use that global state too.
       *
       * Or maybe we can have an interpreter that takes ref cell as argument,
       * and then we can pass our ref cell to it.
       *
       * Hm..
       *)
      .< let c = !current_char in
         assert false >.

  | _ -> assert false

and eval_s1 (exp : exp_s) (cc : char option)
            (cont : exp_s -> char option -> exp_s code) : exp_s code =
  match exp with
  | Backtick_S (arg1, arg2) -> eval_s1 arg1 cc (fun arg1v cc' ->
      match arg1v with
      | D_S -> cont (D1_S arg2) cc'
      | v1  -> eval_s1 arg2 cc' (fun v2 cc'' -> apply_s1 v1 v2 cc'' cont))
  | _ -> cont exp cc

let _ =
  let contents = slurp Sys.argv.(1) in
  let (p, pos) = parse contents in

  if Array.length Sys.argv == 3 && String.compare Sys.argv.(2) "stage" == 0 then
    Print_code.print_code Format.std_formatter (eval_s1 (tr p) None (fun x _ -> .<x>.))
    (* Runcode.(!.) (eval_s1 (tr p) None (fun x _ -> .<x>.)); *)
  else begin
    Printf.printf "last position: %d, string length: %d\n" pos (String.length contents);
    (* FIXME: This assertion fails if program has trailing WS/comments *)
    assert (pos == String.length contents);
    let _ = eval p None (fun x _ -> x) in
    ()
  end
