-- the (previously) untyped expression language of TAPL chapter 3,
--   using GADTs to ensure that invalid terms are unrepresentable

{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies #-}

import Data.Maybe (fromMaybe)

-- summon two meaningless constants from the void
data TNat
data TBool

-- we index terms by their type
data Term a where
  T :: Term TBool
  F :: Term TBool
  IfThenElse :: Term TBool -> Term a -> Term a -> Term a
  Z :: Term TNat
  Succ :: Term TNat -> Term TNat
  Pred :: Term TNat -> Term TNat
  IsZero :: Term TNat -> Term TBool

instance Show (Term a) where
  show T = "T"
  show F = "F"
  show (IfThenElse t1 t2 t3) =
    "(IfThenElse " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3 ++ ")"
  show Z = "Z"
  show (Succ x) = "(Succ " ++ show x ++ ")"
  show (Pred x) = "(Pred " ++ show x ++ ")"
  show (IsZero x) = "(IsZero " ++ show x ++ ")"

-- small-step evaluation rules: no need for care
--   we only have "unstickable" terms, so "no rule applies"
--   means we have a value

eval1 :: Term a -> Maybe (Term a)

-- E-IfTrue
eval1 (IfThenElse T t _) = Just t

-- E-IfFalse
eval1 (IfThenElse F _ f) = Just f

-- E-If
eval1 (IfThenElse x t f) = do
  x' <- eval1 x
  return $ IfThenElse x' t f

-- E-Succ
eval1 (Succ x) = Succ <$> eval1 x

-- E-PredZero
eval1 (Pred Z) = Just Z

-- E-PredSucc
eval1 (Pred (Succ x)) = Just x

-- E-Pred
eval1 (Pred x) = Pred <$> eval1 x

-- E-IsZeroZero
eval1 (IsZero Z) = Just T

-- E-IsZeroSucc
eval1 (IsZero (Succ x)) = Just F

-- E-IsZero
eval1 (IsZero x) = IsZero <$> eval1 x

-- otherwise, we're at a value
eval1 _ = Nothing

-- the big-step is just what happens when we take small-steps until
--   we can't apply any more rules
eval :: Term a -> Term a
eval t = fromMaybe t (eval <$> eval1 t)

-- of course, we can evaluate terms into haskell values of the appropriate type;
--   we'll use MPTCs and fundeps to lock everything down correctly

class EvalHask a b | a -> b where
  evalHask :: Term a -> b

instance EvalHask TBool Bool where
  evalHask T = True
  evalHask F = False
  evalHask x = evalHask (eval x)

instance EvalHask TNat Integer where
  evalHask Z = 0
  evalHask (Succ x) = 1 + evalHask x
  evalHask x = evalHask (eval x)
