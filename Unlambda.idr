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
-- * Parsing

skipComment : IO ()
skipComment = do
  c <- getChar
  if c == '\n' then return () else skipComment

parse : IO Exp
parse = do
  c <- getChar
  case c of
    '#'  => do skipComment; parse
    '`'  => Backtick <$> parse <*> parse
    '.'  => Print <$> getChar
    'r'  => return $ Print '\n'
    'k'  => return K
    's'  => return S
    'i'  => return I
    'v'  => return V
    'c'  => return C
    'd'  => return D
    'e'  => E <$> parse
    '@'  => return Read
    '?'  => Cmp <$> getChar
    '|'  => return Repr
    ' '  => parse
    '\n' => parse
    '\t' => parse
    -- how to error in idris?

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

--------------
-- * Execution

-- | Reference implementation
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
      C      => cont (Cont cont) c
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

partiallyEvaluated : Maybe Char -> (Exp -> Maybe Char -> IO Exp) -> IO Exp
partiallyEvaluated = eval (parseStr hello)

partiallyEvaluated1 : (Exp -> Maybe Char -> IO Exp) -> IO Exp
partiallyEvaluated1 = eval (parseStr hello) Nothing

partiallyEvaluated2 : IO Exp
partiallyEvaluated2 = eval (parseStr hello) Nothing (\e, _ => return e)

partiallyEvaluatedEOF : Maybe Char -> (Exp -> Maybe Char -> IO Exp) -> IO Exp
partiallyEvaluatedEOF = eval (parseStr eofTest)

partiallyEvaluatedEOF1 : (Exp -> Maybe Char -> IO Exp) -> IO Exp
partiallyEvaluatedEOF1 = eval (parseStr eofTest) Nothing

partiallyEvaluatedEOF2 : IO Exp
partiallyEvaluatedEOF2 = eval (parseStr eofTest) Nothing (\e, _ => return e)

partiallyEvaluatedPrint : Maybe Char -> (Exp -> Maybe Char -> IO Exp) -> IO Exp
partiallyEvaluatedPrint = eval (parseStr printTest)

partiallyEvaluatedPrint1 : (Exp -> Maybe Char -> IO Exp) -> IO Exp
partiallyEvaluatedPrint1 = eval (parseStr printTest) Nothing

partiallyEvaluatedPrint2 : IO Exp
partiallyEvaluatedPrint2 = eval (parseStr printTest) Nothing (\e, _ => return e)

loopPE : IO Exp
loopPE = eval (parseStr loopingPgm) Nothing (\e, _ => return e)

-- TODO: It seems like Idris doesn't have records. Make sure.
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

mutual
  apply_cont : [static] PEOpts -> ExpS -> Maybe Char -> List Continuation -> (ExpS, List Continuation)
  apply_cont opts e1 cc conts =
    case conts of
      [] => (e1, [])
      DelayGuard e2 :: rest =>
        case e1 of
          D_S => apply_cont opts (D1_S e2) cc rest
          _   => eval_static opts e2 cc (ApplyTo e1 :: rest)
      ApplyTo f :: rest => apply_static opts f e1 cc rest
      ApplyDelayed a :: rest => apply_static opts e1 a cc rest

  apply_static : [static] PEOpts -> ExpS -> ExpS -> Maybe Char
              -> List Continuation -> (ExpS, List Continuation)
  apply_static opts@(PE evalS evalEOF evalCC) e1 e2 c conts =
    case e1 of
      K_S      => apply_cont opts (K1_S e2) c conts
      K1_S x   => apply_cont opts x c conts
      S_S      => apply_cont opts (S1_S e2) c conts
      S1_S x   => apply_cont opts (S2_S x e2) c conts
      S2_S x y =>
        if evalS
          then eval_static opts (Backtick_S (Backtick_S x e2) (Backtick_S y e2)) c conts
          else
            -- my guess is that partial evaluator will just keep evaluating stuff.
            (Backtick_S (Backtick_S x e2) (Backtick_S y e2), conts)
      I_S      => apply_cont opts e2 c conts
      V_S      => apply_cont opts V_S c conts
      C_S      => apply_static opts e2 (Cont_S conts) c conts
      Cont_S conts' => apply_cont opts e2 c conts'
      D_S      => apply_cont opts e2 c conts
      D1_S f   => eval_static opts f c (ApplyDelayed e2 :: conts)
      Print_S ch =>
        let (e2', conts') = apply_cont opts e2 c conts in
        -- TODO: Should I return Print_S and add e2 to continuations instead?
        (Backtick_S (Print_S ch) e2', conts')
      E_S x    => (x, [])
      Read_S   =>
        -- Finally! I think I've found a limitation here...
        (Backtick_S Read_S e2, conts)
      Cmp_S c' => apply_static opts e2 (if Just c' == c then I_S else V_S) c conts
      Repr_S   => apply_static opts e2 (case c of Nothing => V_S; Just c' => Print_S c') c conts
      Backtick_S => error "Can't apply backtick"

  eval_static : [static] PEOpts -> ExpS -> Maybe Char
             -> List Continuation -> (ExpS, List Continuation)
  eval_static opts exp cc conts =
    case exp of
      Backtick_S e1 e2 => eval_static opts e1 cc (DelayGuard e2 :: conts)
      _                => apply_cont opts exp cc conts

optimizedHello : (ExpS, List Continuation)
optimizedHello = eval_static (PE True True True) (tr $ parseStr hello) Nothing []

main : IO ()
main = do
    _ <- eval (parseStr hello) Nothing (\e, _ => return e)
    putStrLn "Done"
    -- exp <- parse
    -- eval exp Nothing (\e, _ => return e)
    -- -- -- print exp
    -- putStrLn "Done"
