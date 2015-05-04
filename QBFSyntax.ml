(*
 * Syntax for the QBF language described in "DSL Implementation in MetaOCaml,
 * TemplateHaskell and C++":
 * http://www.cs.rice.edu/~taha/publications/journal/dspg04b.pdf
 *
 * Defined in it's own module to be able to use with MetaOCaml.
 *)

type bexp =
  | True                     (* T *)
  | False                    (* F *)
  | And     of bexp * bexp   (* T ^ F *)
  | Or      of bexp * bexp   (* T v T *)
  | Not     of bexp          (* not T *)
  | Implies of bexp * bexp   (* F => T *)
  | Forall  of string * bexp (* forall x. x and not x *)
  | Var     of string        (* x *)
