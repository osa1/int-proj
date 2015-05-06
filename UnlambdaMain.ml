(* Code printing is done here to make CSP values printed better *)

open CmdArgs
open Lift
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

  let input_file : (string option) ref = ref None in
  let usage = "" in

  (* I want my optparse-applicative back ... *)

  (* FIXME: opts is from CmdArgs module, it's a global state *)
  Arg.parse
    [ ( "-staged",
        Arg.Unit (function () -> opts.staged <- true),
        "Run staged interpreter." );

      ( "-compile",
        Arg.Unit (function () -> opts.compile <- true),
        "Compile and run compiled program." );

      ( "-run",
        Arg.Unit (function () -> opts.run <- true),
        "Run specialized code. Makes sense only in staged mode." );

      ( "-parse-only",
        Arg.Unit (function () -> opts.parse_only <- true),
        "Do only parsing in specialization time" );

      ( "-eval-S",
        Arg.Unit (function () -> opts.eval_S <- true),
        "Specialize applications of S. WARNING: Results in loops most of the time." );

      ( "-eval-eof",
        Arg.Unit (function () -> opts.eval_eof <- true),
        "Specialize EOF branches of \"read\"('@') calls." );

      ( "-eval-cc",
        Arg.Unit (function () -> opts.eval_cc <- true),
        "Specialize continuation calls. WARNING: May result in loops." );

    ] (fun annon_arg -> match !input_file with
                        | None -> input_file := Some annon_arg
                        | Some i -> raise (Failure ("Input file is already specified as " ^ i))) usage;

  match !input_file with
  | None -> raise (Failure "Input file is not specified.")
  | Some input_file -> opts.input_file <- input_file;

  let contents = slurp opts.input_file in

  let (p, pos)      = parse_var contents in
  let exp   : exp   = tr_v (abs_elim p) in
  let exp_s : exp_s = tr exp in

  if opts.staged && opts.compile then
    raise (Failure "Both -compile and -staged are provided.");

  if opts.staged then
    begin
      if opts.parse_only then
        Print_code.print_code Format.std_formatter
          .< UnlambdaCont.eval .~ (lift_exp_s exp_s) None [] >.
      else begin
        let code = UnlambdaStaged.eval exp_s None []; in
        Print_code.print_code Format.std_formatter code;
        if opts.run then
          let _ = Runcode.run code in
          ()
      end
    end
  else if opts.compile then
    let _ = Runcode.run (UnlambdaCompiler.compile exp_s) (fun x -> x) in
    ()
  else begin
    Printf.printf "running reference implementation\n";
    Printf.printf "last position: %d, string length: %d\n" pos (String.length contents);
    let _ = UnlambdaInterp.eval exp None (fun x _ -> x) in
    ()
  end
