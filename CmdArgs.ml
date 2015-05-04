(* TODO: Fields are mutable to be able to initialize gradually. Maybe fix this
 * by using intermediate variables instead *)
type cmd_opts =
  { mutable input_file : string;
    mutable staged : bool;              (* use staged interpreters. all the arguments
                                           below this make sense only when this is enabled *)
    mutable run : bool;                 (* run specialized program *)
    mutable parse_only : bool;          (* do only parsing in specialization time *)
    mutable eval_S : bool;              (* evaluate S calls *)
    mutable specialize_eof : bool;      (* specialize None branch of "read"('@') *)
    mutable specialize_cc : bool;       (* specialize continuation calls *)
    mutable specialize_comp_eq : bool;  (* specialize "equal" branches of conditionals *)
    mutable specialize_comp_neq : bool; (* specialize "not equal" branches of conditionals *)
  }

let init_opts input_file =
  { input_file = input_file;
    staged = false;
    run = false;
    parse_only = false;
    eval_S = false;
    specialize_eof = false;
    specialize_cc = false;
    specialize_comp_eq = false;
    specialize_comp_neq = false;
  }

(* FIXME: Remove global state *)
let opts = init_opts ""
