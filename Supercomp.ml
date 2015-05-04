type ('s, 'd) sd =
  { mutable dynamic : 'd code;
    mutable static  : 's option; }

type ('s, 'd) ps_cell =
  | Nil
  | Cons of ('s, 'd) sd * ('s, 'd) psl

and ('s, 'd) psl =
  (('s, 'd) ps_cell, 'd list) sd

let unknown x = { dynamic = x; static = None }

let forget x = x.dynamic

let assuming_eq x v thunk =
  let saved = x.static in
  try x.static <- Some v;
      let ret = thunk () in
      x.static <- saved;
      ret
  with e -> x.static <- saved;
            raise e

let dfun f = .< fun x -> .~ (f (unknown .<x>.)) >.

let match_ls ls for_nil for_cons =
  match ls.static with
  | Some Nil -> for_nil ()
  | Some (Cons (x, xs)) -> for_cons x xs
  | None ->
      .< match .~ ls.dynamic with
         | [] -> .~ (assuming_eq ls Nil for_nil)
         | x :: xs ->
             .~ (let x_d = unknown .<x>. in
                 let xs_d = unknown .<xs>. in
                 assuming_eq ls (Cons (x_d, xs_d)) (fun () -> for_cons x_d xs_d))
       >.
