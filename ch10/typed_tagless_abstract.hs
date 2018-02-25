-- the simply-typed lambda calculus, in typed tagless style
-- variables are represented as De Bruijn indices, and they succ

import Prelude hiding (succ, abs)

class Interp repr where
  varZ :: repr (a, as) a
  varS :: repr c a -> repr (b, c) a
  abs :: repr (a, as) b -> repr as (a -> b)
  app :: repr c (a -> b) -> repr c a -> repr c b
  t :: repr c Bool
  f :: repr c Bool
  ifThenElse :: repr c Bool -> repr c a -> repr c a -> repr c a
  z :: repr c Int
  succ :: repr c Int -> repr c Int
  prev :: repr c Int -> repr c Int
  isZero :: repr c Int -> repr c Bool

newtype Eval c a = Eval { eval :: c -> a }

instance Interp Eval where
  varZ = Eval $ fst
  varS (Eval n) = Eval $ \(_, x) -> n x
  abs (Eval t) = Eval $ \c -> \x -> t (x, c)
  app (Eval t1) (Eval t2) = Eval $ \c -> t1 c (t2 c)
  t = Eval $ const True
  f = Eval $ const False
  ifThenElse (Eval b) (Eval ifT) (Eval ifF) = Eval $ \c ->
    if b c then ifT c else ifF c
  z = Eval $ const 0
  succ (Eval n) = Eval $ \c -> n c + 1
  prev (Eval n) = Eval $ \c -> case n c of
    0 -> 0
    n -> n - 1
  isZero (Eval n) = Eval $ \c -> n c == 0

-- functions from nesting depth to pretty-printed representations
newtype PPrint c a = PPrint { pprint :: Int -> String }

instance Interp PPrint where
  varZ = PPrint $ ("v" ++) . show
  varS (PPrint n) = PPrint $ \i -> n (i - 1)
  abs (PPrint t) = PPrint $ \i -> "(\\. " ++ t (i + 1) ++ ")"
  app (PPrint t1) (PPrint t2) = PPrint $ \i -> "(" ++ t1 i ++ " " ++ t2 i ++ ")"
  t = PPrint $ const "T"
  f = PPrint $ const "F"
  ifThenElse (PPrint b) (PPrint ifT) (PPrint ifF) =
    PPrint $ \i -> "IfThenElse(" ++ b i ++ ", " ++ ifT i ++ ", " ++ ifF i ++ ")"
  z = PPrint $ const "Z"
  succ (PPrint n) = PPrint $ \i -> "Succ(" ++ n i ++ ")"
  prev (PPrint n) = PPrint $ \i -> "Prev(" ++ n i ++ ")"
  isZero (PPrint n) = PPrint $ \i -> "IsZero(" ++ n i ++ ")"

{--
example :: Interp repr => repr () Bool
example = app (abs (app varZ (succ z))) (abs (isZero varZ))
--}

example :: Interp repr => repr () Int
example = app (app (abs (abs $ varS varZ)) (succ z)) (succ (succ z))
-- ((\.\.v1) 1) 2

main :: IO ()
main = do
  putStrLn $ pprint example 0
  print $ eval example ()
