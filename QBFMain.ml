open QBFSyntax
open QBFEval

let _ =
  let generated = eval_staged (Forall ("p", Implies (True, Var "p"))) env0 in
  Print_code.print_code Format.std_formatter generated;
  Printf.printf "\nResult: %b\n" (Runcode.run generated);

  let pgm1 = Forall ("x", (Or (False, And (True, True)))) in
  let generated_nonopt = eval_staged pgm1 env0 in
  let generated_opt = eval_staged (optimize pgm1) env0 in

  Print_code.print_code Format.std_formatter generated_nonopt;
  Print_code.print_code Format.std_formatter generated_opt;

  Printf.printf "\nResult: %b\n" (Runcode.run generated_nonopt);
  Printf.printf "\nResult: %b\n" (Runcode.run generated_opt);
