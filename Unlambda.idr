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

  eval : [static] Exp -> [static] Maybe Char -> [static] (Exp -> Maybe Char -> IO Exp) -> IO Exp
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

loopPE : IO Exp
loopPE = eval (parseStr loopingPgm) Nothing (\e, _ => return e)

main : IO ()
main = do
    _ <- eval (parseStr hello) Nothing (\e, _ => return e)
    putStrLn "Done"
    -- exp <- parse
    -- eval exp Nothing (\e, _ => return e)
    -- -- -- print exp
    -- putStrLn "Done"
