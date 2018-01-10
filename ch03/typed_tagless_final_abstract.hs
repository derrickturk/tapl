-- the (previously) untyped expression language of TAPL chapter 3,
--   using a typed tagless final representation
--   a la http://okmij.org/ftp/tagless-final/course/lecture.pdf

-- look ma, no GADTs or type families

-- this one permits multiple interpreters

import Prelude hiding (succ)

class Interp repr where
  t :: repr Bool
  f :: repr Bool
  ifThenElse :: repr Bool -> repr a -> repr a -> repr a
  z :: repr Int
  succ :: repr Int -> repr Int
  pred :: repr Int -> repr Int
  isZero :: repr Int -> repr Bool

newtype Eval a = Eval { eval :: a }

instance Interp Eval where
  t = Eval True
  f = Eval False

  ifThenElse (Eval True) t _ = t
  ifThenElse (Eval False) _ f = f

  z = Eval 0

  succ (Eval n) = Eval (1 + n)

  pred (Eval n)
    | n == 0 = Eval 0
    | otherwise = Eval (n - 1)

  isZero (Eval n) = Eval (n == 0)

newtype PPrint a = PPrint { pprint :: String }

instance Interp PPrint where
  t = PPrint "t"
  f = PPrint "f"

  ifThenElse (PPrint t1) (PPrint t2) (PPrint t3) =
    PPrint $ "ifThenElse (" ++ t1 ++ ") (" ++ t2 ++ ") (" ++ t3 ++ ")"

  z = PPrint "z"

  succ (PPrint n) = PPrint $ "succ (" ++ n ++ ")"

  pred (PPrint n) = PPrint $ "pred (" ++ n ++ ")"

  isZero (PPrint n) = PPrint $ "isZero (" ++ n ++ ")"
