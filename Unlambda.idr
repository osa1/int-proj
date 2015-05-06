import Control.Catchable
import Control.IOExcept
import Debug.Error
import Debug.Trace

-- We hard-code some programs to be able to partially evaluate evaluators.

hello : String
hello = concat
  [ "`"
  , "``si`k``s.H``s.e``s.l``s.l``s.o``s. "
  , "``s.w``s.o``s.r``s.l``s.d``s.!``sri"
  , "``si``si``si``si``si``si``si``si`ki"
  ]

loopingPgm : String
loopingPgm = concat
  [ "```si``s"
  , "`k`d`r`.!`.l`.a`.i`.v`.i`.r`.t`. `.t`.s`.e`.'`.c`. `.,`.a`.d`.b`.m`.a`.l`.n.U"
  , "i`c``sii"
  ]

eofTest : String
eofTest = "`r``````s`kc``s`k`s`k`kk``ss`k`k`ki`@i.F.Ti"

printTest : String
printTest = "`.1i"

data Exp
  = Backtick Exp Exp
  | K
  | K1 Exp
  | S
  | S1 Exp
  | S2 Exp Exp
  | I
  | V
  | C
  | Cont (Exp -> Maybe Char -> IO Exp)
  | D
  | D1 Exp
  | Print Char
  -- Unlambda 2 extensions
  | E Exp
  | Read
  | Cmp Char
  | Repr

partial
eqExp : Exp -> Exp -> Bool
eqExp (Backtick e1 e2) (Backtick e1' e2') = eqExp e1 e1' && eqExp e2 e2'
eqExp K K = True
eqExp (K1 e) (K1 e') = eqExp e e'
eqExp S S = True
eqExp (S1 e) (S1 e') = eqExp e e'
eqExp (S2 e1 e2) (S2 e1' e2') = eqExp e1 e1' && eqExp e2 e2'
eqExp I I = True
eqExp V V = True
eqExp C C = True
-- eqExp (Cont _) (Cont _) = undefined
eqExp D D = True
eqExp (D1 e) (D1 e') = eqExp e e'
eqExp (Print c1) (Print c2) =
    if c1 == c2 then True else trace ("print chars are not eq: " ++ show c1 ++ ", " ++ show c2) False
eqExp (E e) (E e') = eqExp e e'
eqExp Read Read = True
eqExp (Cmp c1) (Cmp c2) = c1 == c2
eqExp Repr Repr = True
eqExp _ _ = False

------------
-- Parsing

partial
parseStr : String -> Exp
parseStr str = fst $ iter 0
  where
    comment : Int -> Int
    comment i =
      if strIndex str i == '\n' then i + 1 else comment (i + 1)

    iter : Int -> (Exp, Int)
    iter i =
      case strIndex str i of
        '#'  => iter (comment (i + 1))
        '`'  =>
          let (e1, i1) = iter (i + 1)
              (e2, i2) = iter i1
           in (Backtick e1 e2, i2)
        '.'  =>
          let c = strIndex str (i + 1)
           in (Print c, i + 2)
        'r'  => (Print '\n', i + 1)
        'k'  => (K, i + 1)
        's'  => (S, i + 1)
        'i'  => (I, i + 1)
        'v'  => (V, i + 1)
        'c'  => (C, i + 1)
        'd'  => (D, i + 1)
        'e'  =>
          let (e', i') = iter (i + 1)
           in (E e', i')
        '@'  => (Read, i + 1)
        '?'  =>
          let c = strIndex str (i + 1)
           in (Cmp c, i + 2)
        '|'  => (Repr, i + 1)
        ' '  => iter (i + 1)
        '\n' => iter (i + 1)
        '\t' => iter (i + 1)

---------------------------
-- Reference implementation

mutual
  apply : Exp -> Exp -> Maybe Char -> (Exp -> Maybe Char -> IO Exp) -> IO Exp
  apply e1 e2 c cont =
    case e1 of
      K      => cont (K1 e2) c
      K1 x   => cont x c
      S      => cont (S1 e2) c
      S1 x   => cont (S2 x e2) c
      S2 x y => eval (Backtick (Backtick x e2) (Backtick y e2)) c cont
      I      => cont e2 c
      V      => cont V c
      C      => apply e2 (Cont cont) c cont
      Cont cont' => cont' e2 c
      D      => cont e2 c
      D1 f   => eval f c (\v1, c' => apply v1 e2 c' cont)
      Print ch => do putChar ch; cont e2 c -- where's (>>) ?
      E x    => return x
      Read   => do
        c' <- ioe_run (ioe_lift getChar) (\_ => return Nothing) (return . Just)
        apply e2 (case c' of Nothing => V; Just _ => I) c' cont
      Cmp c' => apply e2 (if Just c' == c then I else V) c cont
      Repr   => apply e2 (case c of Nothing => V; Just c' => Print c') c cont
      Backtick => error "Can't apply backtick"

  -- TODO: Explain what happens if we just mark these args as `static`.
  eval : Exp -> Maybe Char -> (Exp -> Maybe Char -> IO Exp) -> IO Exp
  eval (Backtick arg1 arg2) c cont =
      eval arg1 c $ \arg1', c' =>
        case arg1' of
          D  => cont (D1 arg2) c'
          v1 => eval arg2 c' (\v2, c'' => apply v1 v2 c'' cont)
  eval exp c cont = cont exp c

-----------------------------------------------------
-- Syntax and settings for partial evaluation version

data PEOpts = PE
                Bool   -- ^ eval S applications statically
                       --   (most of the time results in loops)
                Bool   -- ^ eval EOF cases of "read"('@') statically
                Bool   -- ^ eval continuation applications statically
                       --   (may result in loops)

-- We need a different representation of continuations. (TODO: document this)
-- It'd be great if I could solve this typing problem.. It seems like Idris
-- doesn't have type synonyms and I don't want to introduce a newtype, because
-- I don't want to do wrapping/unwrapping manually in million of places.

mutual
  data Continuation
    = DelayGuard ExpS
    | ApplyTo ExpS
    | ApplyDelayed ExpS

  data ExpS
    = Backtick_S ExpS ExpS
    | K_S
    | K1_S ExpS
    | S_S
    | S1_S ExpS
    | S2_S ExpS ExpS
    | I_S
    | V_S
    | C_S
    | Cont_S (List Continuation)
    | D_S
    | D1_S ExpS
    | Print_S Char
    -- Unlambda 2 extensions
    | E_S ExpS
    | Read_S
    | Cmp_S Char
    | Repr_S

tr : Exp -> ExpS
tr (Backtick e1 e2) = Backtick_S (tr e1) (tr e2)
tr K = K_S
tr (K1 e) = K1_S (tr e)
tr S = S_S
tr (S1 e) = S1_S (tr e)
tr (S2 e1 e2) = S2_S (tr e1) (tr e2)
tr I = I_S
tr V = V_S
tr C = C_S
tr D = D_S
tr (D1 e) = D1_S (tr e)
tr (Print c) = Print_S c
tr (E e) = E_S (tr e)
tr Read = Read_S
tr (Cmp c) = Cmp_S c

------------------------------------------------------------------
-- Interpreter for the alternative representation of continuations

mutual
  apply_cont : ExpS -> Maybe Char ->  List Continuation -> IO ExpS
  apply_cont e1 cc conts =
    case conts of
      [] => return e1
      DelayGuard e2 :: rest =>
        case e1 of
          D_S => apply_cont (D1_S e2) cc rest
          _   => eval1 e2 cc (ApplyTo e1 :: rest)
      ApplyTo f :: rest => apply1 f e1 cc rest
      ApplyDelayed a :: rest => apply1 e1 a cc rest

  apply1 : ExpS -> ExpS -> Maybe Char ->  List Continuation -> IO ExpS
  apply1 e1 e2 c conts =
    case e1 of
      K_S      => apply_cont (K1_S e2) c conts
      K1_S x   => apply_cont x c conts
      S_S      => apply_cont (S1_S e2) c conts
      S1_S x   => apply_cont (S2_S x e2) c conts
      S2_S x y => eval1 (Backtick_S (Backtick_S x e2) (Backtick_S y e2)) c conts
      I_S      => apply_cont e2 c conts
      V_S      => apply_cont V_S c conts
      C_S      => apply1 e2 (Cont_S conts) c conts
      Cont_S conts' => apply_cont e2 c conts'
      D_S      => apply_cont e2 c conts
      D1_S f   => eval1 f c (ApplyDelayed e2 :: conts)
      Print_S ch => do putChar ch; apply_cont e2 c conts
      E_S x    => return x
      Read_S   => do
        c' <- ioe_run (ioe_lift getChar) (\_ => return Nothing) (return . Just)
        apply1 e2 (case c' of Nothing => V_S; Just _ => I_S) c' conts
      Cmp_S c' => apply1 e2 (if Just c' == c then I_S else V_S) c conts
      Repr_S   => apply1 e2 (case c of Nothing => V_S; Just c' => Print_S c') c conts
      Backtick_S => error "Can't apply backtick"

  eval1 : ExpS -> Maybe Char -> List Continuation -> IO ExpS
  eval1 exp cc conts =
    case exp of
      Backtick_S e1 e2 => eval1 e1 cc (DelayGuard e2 :: conts)
      _                => apply_cont exp cc conts

-------------------------------------------------
-- Optimizer that's supposed to run in P.E. time.

-- See the call site for explanation
eofCond : ExpS -> ExpS -> ExpS
eofCond t f =
  Backtick_S
    (Print_S '\n')
    (Backtick_S (Backtick_S (Backtick_S (Backtick_S (Backtick_S
      (Backtick_S S_S (Backtick_S K_S C_S))
      (Backtick_S (Backtick_S S_S (Backtick_S K_S (Backtick_S S_S (Backtick_S K_S
        (Backtick_S K_S K_S)))))
        (Backtick_S (Backtick_S S_S S_S) (Backtick_S K_S (Backtick_S K_S (Backtick_S K_S I_S))))))
          (Backtick_S Read_S I_S)) t) f) I_S)

mutual
  apply_cont_static : [static] PEOpts -> [static] ExpS -> [static] Maybe Char
            -> [static] List Continuation -> (ExpS, List Continuation)
  apply_cont_static opts e1 cc conts =
    case conts of
      [] => (e1, [])
      DelayGuard e2 :: rest =>
        case e1 of
          D_S => apply_cont_static opts (D1_S e2) cc rest
          _   => eval_static opts e2 cc (ApplyTo e1 :: rest)
      ApplyTo f :: rest => apply_static opts f e1 cc rest
      ApplyDelayed a :: rest => apply_static opts e1 a cc rest

  apply_static : [static] PEOpts -> [static] ExpS -> [static] ExpS -> [static] Maybe Char
              -> [static] List Continuation -> (ExpS, List Continuation)
  apply_static opts@(PE evalS evalEOF evalCC) e1 e2 c conts =
    case e1 of
      K_S      => apply_cont_static opts (K1_S e2) c conts
      K1_S x   => apply_cont_static opts x c conts
      S_S      => apply_cont_static opts (S1_S e2) c conts
      S1_S x   => apply_cont_static opts (S2_S x e2) c conts
      S2_S x y =>
        if evalS
          then eval_static opts (Backtick_S (Backtick_S x e2) (Backtick_S y e2)) c conts
          else
            -- my guess is that partial evaluator will just keep evaluating stuff.
            (Backtick_S (Backtick_S x e2) (Backtick_S y e2), conts)
      I_S      => apply_cont_static opts e2 c conts
      V_S      => apply_cont_static opts V_S c conts
      C_S      => apply_static opts e2 (Cont_S conts) c conts
      Cont_S conts' =>
        if evalCC then apply_cont_static opts e2 c conts'
                  else (e2, conts')
      D_S      => apply_cont_static opts e2 c conts
      D1_S f   => eval_static opts f c (ApplyDelayed e2 :: conts)
      Print_S ch =>
        -- Let's think about what to do here: We should generate a code that
        -- prints the character, and then continues evaluating the argument.
        -- The problem is, in a compiler from object language to object
        -- language, this is not possible, unless we have a AST node that says
        -- exactly that. I think this is one of the limitation of partial
        -- evaluation + staged interpreters approach(Sperber and Thiemann '96).
        -- In a multi-stage language, we compile the AST representation to meta
        -- language, and we can mix meta language terms to our compiled
        -- code, so it's possible to compile this term to:
        --
        -- > print_char ch; evaluate <rest>
        --
        -- Indeed, this is exactly what we're doing in MetaOCaml implementation.
        --
        -- Luckily, we can build a term that says exactly "print this and evaluate
        -- that", only now we'll have "that" part already evaluated a bit in
        -- partial evaluation time:
        --
        let (e2', conts') = apply_cont_static opts e2 c conts in
        (e2', ApplyTo (Print_S ch) :: conts')
        --
        -- This worked fine, but as we'll see in Read_S case, it's not always
        -- as simple.
        --
      E_S x    => (x, [])
      Read_S   =>
        if evalEOF
           then
             let (eofCase, eofConts) = eval_static opts e2 Nothing conts
             in -- We need to generate a term in object level which will mean
                -- "read a character, evaluate this if it's EOF, evaluate that
                -- otherwise"
                --
                -- Doing this in object language level is not easy, because it
                -- doesn't have booleans, conditionals etc. So we build a term
                -- that emulates all this and means same thing. (see `eofCond`
                -- for the definition)
                --
                -- The obvious problem is that this term is big, and requires
                -- a lot of reductions. Instead of checking a condition in
                -- interpreter, we know created a term that emulates same
                -- conditional in object language level, which means a lot of
                -- reductions from the interpreter side.
                --
                -- If we didn't do a lot of reductions in the EOF case, then we
                -- probably just de-optimized the program. A single step to decide
                -- which branch to take is now compiled to a 20+ steps.
                -- (our new term includes more than 20 backticks)
                --
                -- Another issue is that we need to evaluate EOF case with
                -- different continuations. So this would be wrong:
                --
                -- (eofCond eofCase e2, conts)
                --
                -- Because specialized case will have different continuations.
                -- So we should replace it with a continuation call:
                --
                (eofCond (Backtick_S (Cont_S eofConts) eofCase) e2, conts)
                -- I think Print_S and Read_S cases demonstrate one of the
                -- limitations of specializing interpreters using partial
                -- evaluators.
           else (Backtick_S Read_S e2, conts)
      Cmp_S c' => apply_static opts e2 (if Just c' == c then I_S else V_S) c conts
      Repr_S   => apply_static opts e2 (case c of Nothing => V_S; Just c' => Print_S c') c conts
      Backtick_S => error "Can't apply backtick"

  eval_static : [static] PEOpts -> [static] ExpS -> [static] Maybe Char
             -> [static] List Continuation -> (ExpS, List Continuation)
  eval_static opts exp cc conts =
    case exp of
      Backtick_S e1 e2 => eval_static opts e1 cc (DelayGuard e2 :: conts)
      _                => apply_cont_static opts exp cc conts

optimizedHello : (ExpS, List Continuation)
optimizedHello = eval_static (PE True True True) (tr $ parseStr hello) Nothing []

main : IO ()
main = do
    _ <- eval (parseStr hello) Nothing (\e, _ => return e)
    putStrLn "Done"

-------------------------------
-- Partially evaluated programs

peHello : IO Exp
peHello = eval (parseStr hello) Nothing (\e, _ => return e)

peHello1 : IO ExpS
peHello1 =
    let (exp, conts) = eval_static (PE True True True) (tr $ parseStr hello) Nothing []
    in eval1 exp Nothing conts

peLoop : IO Exp
peLoop = eval (parseStr loopingPgm) Nothing (\e, _ => return e)

peLoopTerminateTry : IO ExpS
peLoopTerminateTry =
    let (exp, conts) = eval_static (PE False True True) (tr $ parseStr loopingPgm) Nothing []
    in eval1 exp Nothing conts

-- Optimize non-terminating program. Loops forever, because we evaluate S applications.
peLoopTerminateOptLoop : (ExpS, List Continuation)
peLoopTerminateOptLoop = eval_static (PE True True True) (tr $ parseStr loopingPgm) Nothing []

-- Optimize non-termianting program. Terminates because we don't evaluate S applications.
peLoopTerminateOptDon'tLoop : (ExpS, List Continuation)
peLoopTerminateOptDon'tLoop = eval_static (PE False True True) (tr $ parseStr loopingPgm) Nothing []

peLoop1 : IO ExpS
peLoop1 =
    let (exp, conts) = eval_static (PE True True True) (tr $ parseStr loopingPgm) Nothing []
    in eval1 exp Nothing conts

peEOFTest : IO Exp
peEOFTest = eval (parseStr eofTest) Nothing (\e, _ => return e)

peEOFTest1 : IO ExpS
peEOFTest1 =
    let (exp, conts) = eval_static (PE True True True) (tr $ parseStr eofTest) Nothing []
    in eval1 exp Nothing conts

pePrint : IO Exp
pePrint = eval (parseStr printTest) Nothing (\e, _ => return e)

pePrint1 : IO ExpS
pePrint1 =
    let (exp, conts) = eval_static (PE True True True) (tr $ parseStr printTest) Nothing []
    in eval1 exp Nothing conts
