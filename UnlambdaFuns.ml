let current_char : (char option) ref = ref None

let rec k e cont = cont k1
and k1 e1 _ cont = cont e1
and s e cont = cont (s1 e)
and s1 e1 e2 cont = cont (s2 e1 e2)
and s2 x y z cont = cont (x z) (y z)
and i x cont = cont x
and v x cont = cont v (* we need -rectypes for this *)
and cc x cont = x cont
and d x cont = cont x
and d1 f arg cont = f (fun v -> v arg cont)
and print c x cont = print_char c; cont x
and e x _ cont = x
and read x cont =
      let _ = (current_char := try Some (input_char stdin)
                               with _ -> None) in
      x (match !current_char with
         | None -> v
         | Some _ -> i) cont
and cmp c x cont =
  x (match !current_char with
     | None -> v
     | Some c -> print c) cont
and repr x cont =
  x (match !current_char with
     | None -> v
     | Some c -> print c) cont
