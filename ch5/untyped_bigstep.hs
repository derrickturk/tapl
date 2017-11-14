-- the untyped lambda calculus, using de Bruijn indices,
--   with a big-step evaluator

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import qualified Data.Set as S

data Term = Var Int
          | Abs Term String -- the string is a "name hint" for pprinting
          | App Term Term
          deriving Show

data NamedTerm = NVar String
               | NAbs String NamedTerm
               | NApp NamedTerm NamedTerm
               deriving Show

isValue :: Term -> Bool
isValue (Abs _ _) = True
isValue _ = False

subst :: Int -> Term -> Term -> Term
subst x what (Var n)
  | x == n = what
  | otherwise = (Var n)
subst x what (Abs t n) = Abs (subst (x + 1) (shift 1 what) t) n
subst x what (App t1 t2) = App (subst x what t1) (subst x what t2)

shift :: Int -> Term -> Term
shift = shift' 0 where
  shift' cutoff s (Var n)
    | n < cutoff = Var n
    | otherwise = Var (n + s)
  shift' cutoff s (Abs t n) = Abs (shift' (cutoff + 1) s t) n
  shift' cutoff s (App t1 t2) = App (shift' cutoff s t1) (shift' cutoff s t2)

eval :: Term -> Term
eval a@(App t1 t2) = case eval t1 of
  Abs t n -> eval $ shift (-1) $ subst 0 (shift 1 (eval t2)) t
  _ -> a
eval x = x

evalNamed :: NamedTerm -> NamedTerm
evalNamed t = let (t', ctxt) = removeNames t in restoreNames (eval t') ctxt

freeVariables :: NamedTerm -> S.Set String
freeVariables = go S.empty where
  go bound (NVar x) = if S.member x bound then S.empty else S.singleton x
  go bound (NAbs x t) = go (S.insert x bound) t
  go bound (NApp t1 t2) = go bound t1 `S.union` go bound t2

removeNames :: NamedTerm -> (Term, [String])
removeNames t = (fromMaybe (error "impossible") (go t ctxt), ctxt) where
  ctxt = S.toList $ freeVariables t
  go (NVar n) ctxt = Var <$> elemIndex n ctxt
  go (NAbs n t) ctxt = Abs <$> go t (n:ctxt) <*> pure n
  go (NApp t1 t2) ctxt = App <$> go t1 ctxt <*> go t2 ctxt

restoreNames :: Term -> [String] -> NamedTerm
restoreNames (Var n) ctxt = NVar $ ctxt !! n
restoreNames (Abs t n) ctxt = NAbs n' (restoreNames t (n':ctxt)) where
  n' = head $ filter (not . (`elem` ctxt)) (iterate (++ "'") n)
restoreNames (App t1 t2) ctxt =
  NApp (restoreNames t1 ctxt) (restoreNames t2 ctxt)
