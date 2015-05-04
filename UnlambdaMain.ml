(* Code printing is done here to make CSP values printed better *)

open Syntax
open Lift

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

(* Some hand-rolled unit tests *)
let run_tests () =
  let eqs =
    [ ("\\x -> $x ", "i")
    ; ("\\x -> i", "`ki")
    ; ("\\x -> ` $x k", "``si`kk")
    ; ("\\x -> \\y -> ` $y $x ", (* FIXME: Bug: Not checking EOF,
                                    so we have to put an extra space here. *)
       "``s``s`ks`ki``s`kki")
    ] in
  let evals = [ ] in
  List.iter (function (l, r) ->
    let eliminated = tr_v (abs_elim (fst @@ parse_var l)) in
    let expected   = fst @@ parse r in
    if eliminated <> expected then
      raise (Failure ("Eliminated form is not equal to expected form.\n" ^
                      "Eliminated: " ^ exp_to_string eliminated ^ "\n" ^
                      "Expected: " ^ exp_to_string expected))) eqs;

  List.iter (function (l, r) ->
    let eliminated = tr_v (abs_elim (fst @@ parse_var l)) in
    let expected   = fst @@ parse r in
    let evaluated  = trb (UnlambdaCont.eval (tr eliminated) None []) in
    if evaluated <> expected then
      raise (Failure ("Evaluated form is not equal to expected form.\n" ^
                      "Evaluated: " ^ exp_to_string evaluated ^ "\n" ^
                      "Expected: " ^ exp_to_string expected))) evals;

  ()

let _ =
  (* I don't want program to run if tests fail *)
  run_tests ();

  let contents = slurp Sys.argv.(1) in

  let (p, pos) = parse_var contents in
  let exp      = tr_v (abs_elim p) in
  let exp_s    = tr exp in

  match Array.to_list Sys.argv with
  | [_; _; "stage0"] ->
      Print_code.print_code Format.std_formatter .< UnlambdaCont.eval .~ (lift_exp_s exp_s) None [] >.;
  | (_ :: _ :: "stage" :: _) ->
      let _ = Print_code.print_code Format.std_formatter (UnlambdaStaged.eval exp_s None []) in
      if Array.length Sys.argv == 4 && String.compare Sys.argv.(3) "run" == 0 then
        let _ = Runcode.(!.) (UnlambdaStaged.eval exp_s None []) in ()
  | [_; _; "cont"] ->
      let _ = UnlambdaCont.eval exp_s None [] in ()
  | _ ->
      Printf.printf "running reference implementation\n";
      Printf.printf "last position: %d, string length: %d\n" pos (String.length contents);
      let _ = UnlambdaInterp.eval exp None (fun x _ -> x) in ()
