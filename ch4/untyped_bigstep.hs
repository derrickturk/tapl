-- exercise 4.2.2: a big-step evaluator for the untyped arithmetic language

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

-- big-step evaluation rules
--   we're imposing an evaluation strategy and order by how these
--   rules are framed

eval :: Term -> Term

-- B-Value
eval v
  | isValue v = v

-- B-IfTrue, B-IfFalse
eval ite@(IfThenElse b t f) = case eval b of
  T -> eval t
  F -> eval f
  _ -> ite

-- B-Succ
eval sx@(Succ x) = let x' = eval x in
  if isNumeric x' then (Succ x') else sx

-- B-PredZero, B-PredSucc
eval px@(Pred x) = case eval x of
  Z -> Z
  (Succ x)
    | isNumeric x -> x
  _ -> px

-- B-IsZeroZero, B-IsZeroSucc
eval izx@(IsZero x) = case eval x of
  Z -> T
  (Succ x)
    | isNumeric x -> F
  _ -> izx

-- either a Left stuck or a Right value
evalEither :: Term -> Either Term Term
evalEither t = let t' = eval t in
  if isValue t'
    then Right t'
    else Left t'
