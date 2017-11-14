-- the untyped lambda calculus, before enrichment

import Data.Maybe (fromMaybe)

type VarName = String

data Term = Var VarName
          | Abs VarName Term
          | App Term Term
          deriving Show

-- a predicate for values
isValue :: Term -> Bool
isValue (Abs _ _) = True
isValue _ = False

-- small-step evaluation rules; CBV evaluation order
--   Nothing == could not reduce

eval1 :: Term -> Maybe Term

-- we have to match these in backwards order...

-- E-AppAbs
eval1 (App (Abs var t12) v2)
  | isValue v2 = Just $ subst var v2 t12

-- E-App2
eval1 (App v1 t2)
  | isValue v1 = App v1 <$> eval1 t2

-- E-App1
eval1 (App t1 t2) = flip App t2 <$> eval1 t1

-- otherwise
eval1 _ = Nothing

-- beta-reduction:
subst :: VarName -> Term -> Term -> Term

subst name newTerm (Var n)
  | n == name = newTerm
  | otherwise = Var n

subst name newTerm app@(Abs n t)
  | n == name || free n newTerm =
    subst name newTerm (Abs newName (replace n newName t))
  | otherwise = Abs n (subst name newTerm t)
  where
    newName = n ++ "'"
    replace name newName (Var x)
      | x == name = Var newName
      | otherwise = Var x
    replace name newName (Abs x t)
      | x == name = Abs newName (replace name newName t)
      | otherwise = Abs x (replace name newName t)
    replace name newName (App t1 t2) =
      App (replace name newName t1) (replace name newName t2)

subst name newTerm (App t1 t2) =
  App (subst name newTerm t1) (subst name newTerm t2)

-- is a variable free in a term?
free :: VarName -> Term -> Bool
free n (Var x) = n == x
free n (Abs x t)
  | n == x = False
  | otherwise = free n t
free n (App t1 t2) = free n t1 || free n t2

-- the big-step is just what happens when we take small-steps until
--   we can't apply any more rules
eval :: Term -> Term
eval t = fromMaybe t (eval <$> eval1 t)
