(*
 * Reference implementation of Unlambda: A CPS style interpreter to make
 * call/cc easier to implement. Doesn't do any specialization or optimizations.
 *)

open Syntax

let rec apply (e1 : exp) (e2 : exp) (cc : char option) (cont : exp -> char option -> exp) : exp =
  match e1 with
  | K -> cont (K1 e2) cc
  | K1 x -> cont x cc
  | S -> cont (S1 e2) cc
  | S1 x -> cont (S2 (x, e2)) cc
  | S2 (x, y) -> eval (Backtick (Backtick (x, e2), Backtick (y, e2))) cc cont
  | I -> cont e2 cc
  | V -> cont V cc
  | C -> apply e2 (Cont cont) cc cont
  | Cont c -> c e2 cc
  | D -> cont e2 cc
  | D1 f -> eval f cc (fun v1 cc' -> apply v1 e2 cc' cont)
  | Print c -> print_char c; flush stdout; cont e2 cc
  | E x -> x
  | Read ->
      let c = try Some (input_char stdin)
              with _ -> None in
      apply e2 (match c with None -> V | Some _ -> I) c cont
  | Cmp c ->
      apply e2 (if Some c = cc then I else V) cc cont
  | Repr ->
      apply e2 (match cc with None -> V | Some c -> Print c) cc cont
  | Backtick (e1, e2) -> raise (Failure "can't apply to backtick")

and eval (exp : exp) (cc : char option) (cont : exp -> char option -> exp) : exp =
  match exp with
  | Backtick (arg1, arg2) ->
      eval arg1 cc (fun arg1v cc' ->
        match arg1v with
        | D  -> cont (D1 arg2) cc'
        | v1 -> eval arg2 cc' (fun v2 cc'' -> apply v1 v2 cc'' cont))
  | _ -> cont exp cc
