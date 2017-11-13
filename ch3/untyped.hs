-- the untyped expression language of TAPL chapter 3

import Data.Maybe (fromMaybe)

data Term = T
          | F
          | IfThenElse Term Term Term
          | Z
          | Succ Term
          | Pred Term
          | IsZero Term
          deriving Show

-- a predicate for values
isValue :: Term -> Bool
isValue T = True
isValue F = True
isValue Z = True
isValue (Succ x) = isNumeric x
isValue _ = False

-- we need this to properly enforce the "succ nv" value
isNumeric :: Term -> Bool
isNumeric Z = True
isNumeric (Succ x) = isNumeric x
isNumeric _ = False

-- small-step evaluation rules, very carefully
--   we're imposing an evaluation strategy and order by how these
--   rules are framed
-- we're going to use Maybe to model the possibility of "stuckness",
--   with Nothing representing "No rule applies."

eval1 :: Term -> Maybe Term
-- n.b. we don't have a single-step evaluation rules for
--   values; values are exactly terms that are valid final results
--   of evaluation

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
eval1 (Pred (Succ x))
  | isNumeric x = Just x

-- E-Pred
eval1 (Pred x) = Pred <$> eval1 x

-- E-IsZeroZero
eval1 (IsZero Z) = Just T

-- E-IsZeroSucc
eval1 (IsZero (Succ x))
  | isNumeric x = Just F

-- E-IsZero
eval1 (IsZero x) = IsZero <$> eval1 x

-- otherwise, we're either stuck or at a value
eval1 _ = Nothing

-- the big-step is just what happens when we take small-steps until
--   we can't apply any more rules
eval :: Term -> Term
eval t = fromMaybe t (eval <$> eval1 t)

-- either a Left stuck or a Right value
evalEither :: Term -> Either Term Term
evalEither t = let t' = eval t in
  if isValue t'
    then Right t'
    else Left t'
