(* I could delete half of this file if I could use Haskell.. (or knew OCaml better) *)

(* TODO: Here's a typing problem:
 *
 * We have a syntax tree with continuations, we want to abstract it over
 * continuation types:
 *
 * type cont = Cont
 *
 * type exp_1 =
 *   | Cont_1 of (exp_1 -> exp_1)
 *
 * type exp_2 =
 *   | Cont_2 of cont list
 *
 * So we parameterize it as:
 *
 * type 'cont exp_p =
 *   | Cont of 'cont
 *
 * Now exp_2 is fine:
 *
 * type exp_2_i = (cont list) exp_p
 *
 * But I can't instantiate first case without recursive types:
 *
 * type cont_1  = (cont_1 exp_p -> cont_1 exp_p)
 * type exp_1_i = cont_1 exp_p
 *
 * Which are not allowed in OCaml.
 *
 *)

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

(* syntax tree for staged interpreter, only difference is continuations, we need
 * a different representation of continuations, because depending on whether
 * we're running the specializer or the interpreter(in case of a fallback) we'll
 * need to do different things on the continuations *)

(* TODO: Also talk about how did we come up with these continuations. *)

type cont =
  | DelayGuard of exp_s   (* depending on the result delay second argument *)
  | ApplyTo of exp_s      (* apply to this expression *)
  | ApplyDelayed of exp_s (* apply function to this argument *)

and exp_s =
  | Backtick_S of exp_s * exp_s
  | K_S
  | K1_S       of exp_s
  | S_S
  | S1_S       of exp_s
  | S2_S       of exp_s * exp_s
  | I_S
  | V_S
  | C_S
  | Cont_S     of (cont list)
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

let rec trb (exp : exp_s) : exp =
  match exp with
  | Backtick_S (e1, e2) -> Backtick (trb e1, trb e2)
  | K_S -> K
  | K1_S e -> K1 (trb e)
  | S_S -> S
  | S1_S e -> S1 (trb e)
  | S2_S (e1, e2) -> S2 (trb e1, trb e2)
  | I_S -> I
  | V_S -> V
  | C_S -> C
  | D_S -> D
  | D1_S e -> D1 (trb e)
  | Print_S c -> Print c
  | E_S e -> E (trb e)
  | Read_S -> Read
  | Cmp_S c -> Cmp c
  | Repr_S -> Repr
  | _ -> assert false

let rec exp_to_string = function
  | Backtick (e1, e2) -> "`" ^ exp_to_string e1 ^ exp_to_string e2
  | K -> "k"
  | K1 e -> "`k" ^ exp_to_string e
  | S -> "s"
  | S1 e -> "`s" ^ exp_to_string e
  | S2 (e1, e2) -> "``s" ^ exp_to_string e1 ^ exp_to_string e2
  | I -> "i"
  | V -> "v"
  | C -> "c"
  | Cont _ -> raise (Failure "Can't show continuation.")
  | D -> "d"
  | D1 e -> "`d" ^ exp_to_string e
  | Print c -> "." ^ Char.escaped c
  | E e -> "`e" ^ exp_to_string e
  | Read -> "@"
  | Cmp c -> "?" ^ Char.escaped c
  | Repr -> "|"

let cont_to_string = function
  | DelayGuard e -> "DelayGuard of " ^ exp_to_string (trb e)
  | ApplyTo e -> "ApplyTo of " ^ exp_to_string (trb e)
  | ApplyDelayed e -> "ApplyDelayed of " ^ exp_to_string (trb e)

let rec cont_list_to_string = function
  | []      -> "]"
  | [last]  -> cont_to_string last ^ "]"
  | c :: cs -> cont_to_string c ^ ", " ^ cont_list_to_string cs

(* This syntax is added to make writing Unlambda programs easier:
 * It includes lambdas and variables. A translation step then translates it to
 * standard Unlambda 2 *)
type exp_var =
  | Backtick_V   of exp_var * exp_var
  | K_V
  | K1_V         of exp_var
  | S_V
  | S1_V         of exp_var
  | S2_V         of exp_var * exp_var
  | I_V
  | V_V
  | C_V
  | D_V
  | D1_V         of exp_var
  | Print_V      of char
  (* Unlambda 2 extensions *)
  | E_V          of exp_var
  | Read_V
  | Cmp_V        of char
  | Repr_V
  (* Lambda and variable *)
  | Lambda       of string * exp_var (* \<var> -> <exp_var> *)
  | Var          of string           (* \$[a-z]+ *)

let parse_var (str : string) : (exp_var * int) =
  let rec skip_comment idx =
    match String.get str idx with
    | '\n' -> idx + 1
    | _    -> skip_comment (idx + 1)
  in

  let rec skip_ws idx =
    match String.get str idx with
    | ' ' | '\t' | '\n' -> skip_ws (idx + 1)
    | _ -> idx
  in

  let rec read_var idx =
    match String.get str idx with
    | c when c >= 'a' && c <= 'z' ->
        let (rest, idx1) = read_var (idx + 1) in
        (Char.escaped c ^ rest, idx1)
    | _ -> ("", idx)
  in

  let rec read_arg idx =
    match String.get str idx with
    | ' ' -> ("", skip_ws (skip_ws (idx + 1) + 2))
    | '-' -> ("", skip_ws (idx + 2))
    | c   ->
        let (var, idx1) = read_var idx in
        (var, skip_ws (skip_ws idx1 + 2))
  in

  let rec loop idx =
    match String.get str idx with
    | '#' -> loop (skip_comment (idx + 1))
    | '`' ->
        let (e1, idx1) = loop (idx  + 1) in
        let (e2, idx2) = loop idx1 in
        (Backtick_V (e1, e2), idx2)
    | '.' ->
        let c = String.get str (idx + 1) in
        (Print_V c, idx + 2)
    | 'r' -> (Print_V '\n', idx + 1)
    | 'k' -> (K_V, idx + 1)
    | 's' -> (S_V, idx + 1)
    | 'i' -> (I_V, idx + 1)
    | 'v' -> (V_V, idx + 1)
    | 'c' -> (C_V, idx + 1)
    | 'd' -> (D_V, idx + 1)
    | 'e' ->
        let (e, idx1) = loop (idx + 1) in
        (E_V e, idx1)
    | '@' -> (Read_V, idx + 1)
    | '?' ->
        let c = String.get str (idx + 1) in
        (Cmp_V c, idx + 2)
    | '|' -> (Repr_V, idx + 1)
    | ' ' | '\n' | '\t' -> loop (idx + 1)

    | '\\' ->
        let (arg, idx1) = read_arg (idx + 1) in
        let (body, idx2) = loop idx1 in
        (Lambda (arg, body), idx2)

    | '$' ->
        let (var, idx1) = read_var (idx + 1) in
        (Var var, idx1)

    | chr -> raise (Failure ("Invalid char found: " ^ Char.escaped chr))
  in
  loop 0

let rec abs_elim (exp : exp_var) : exp_var =
  let rec elim_lambda (arg : string) (body : exp_var) : exp_var =
    match body with
    | Var arg1 when arg1 = arg -> I_V
    | Backtick_V (e1, e2) -> Backtick_V (Backtick_V (S_V, elim_lambda arg e1), elim_lambda arg e2)
    | K1_V e -> Backtick_V (K_V, K1_V (elim_lambda arg e))
    | S1_V e -> Backtick_V (K_V, S1_V (elim_lambda arg e))
    | S2_V (e1, e2) -> Backtick_V (K_V, S2_V (elim_lambda arg e1, elim_lambda arg e2))
    | D1_V e -> Backtick_V (K_V, D1_V (elim_lambda arg e))
    | E_V e -> Backtick_V (K_V, E_V (elim_lambda arg e))
    | Lambda (arg1, body1) -> elim_lambda arg (elim_lambda arg1 body1)
    | _ -> Backtick_V (K_V, body)
  in

  match exp with
  | Backtick_V (e1, e2) -> Backtick_V (abs_elim e1, abs_elim e2)
  | K_V -> K_V
  | K1_V e -> K1_V (abs_elim e)
  | S_V -> S_V
  | S1_V e -> S1_V (abs_elim e)
  | S2_V (e1, e2) -> S2_V (abs_elim e1, abs_elim e2)
  | I_V -> I_V
  | V_V -> V_V
  | C_V -> C_V
  | D_V -> D_V
  | D1_V e -> D1_V (abs_elim e)
  | Print_V c -> Print_V c
  | E_V e -> E_V (abs_elim e)
  | Read_V -> Read_V
  | Cmp_V c -> Cmp_V c
  | Repr_V -> Repr_V
  | Lambda (arg, body) -> elim_lambda arg body
  | Var str -> raise (Failure ("abs_elim: Found var: " ^ str))

let rec tr_v (exp : exp_var) : exp =
  match exp with
  | Backtick_V (e1, e2) -> Backtick (tr_v e1, tr_v e2)
  | K_V -> K
  | K1_V e -> K1 (tr_v e)
  | S_V -> S
  | S1_V e -> S1 (tr_v e)
  | S2_V (e1, e2) -> S2 (tr_v e1, tr_v e2)
  | I_V -> I
  | V_V -> V
  | C_V -> C
  | D_V -> D
  | D1_V e -> D1 (tr_v e)
  | Print_V c -> Print c
  | E_V e -> E (tr_v e)
  | Read_V -> Read
  | Cmp_V c -> Cmp c
  | Repr_V -> Repr
  | Lambda _ -> raise (Failure "tr_v: Found lambda")
  | Var str -> raise (Failure ("tr_v: Found var: " ^ str))
