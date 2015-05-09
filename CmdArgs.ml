(* TODO: Fields are mutable to be able to initialize gradually. Maybe fix this
 * by using intermediate variables instead *)
type cmd_opts =
  { mutable input_file : string;
    mutable staged : bool;              (* use staged interpreters. all the arguments
                                           below this make sense only when this is enabled *)
    mutable compile : bool;
    mutable run : bool;                 (* run specialized program *)
    mutable parse_only : bool;          (* do only parsing in specialization time *)
    mutable eval_S : bool;              (* evaluate S calls *)
    mutable eval_eof : bool;            (* specialize None branch of "read"('@') *)
    mutable eval_cc : bool;             (* specialize continuation calls *)
    mutable partial_eval : bool;        (* partially evaluate programs that reads input *)
  }

let init_opts input_file =
  { input_file = input_file;
    staged = false;
    compile = false;
    run = false;
    parse_only = false;
    eval_S = false;
    eval_eof = false;
    eval_cc = false;
    partial_eval = false;
  }

(* FIXME: Remove global state *)
let opts = init_opts ""
