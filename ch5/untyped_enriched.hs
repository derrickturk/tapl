-- the untyped lambda calculus, using de Bruijn indices,
--   with a big-step evaluator, E N R I C H E D with booleans and nats

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import qualified Data.Set as S

data Term = Var Int
          | Abs Term String -- the string is a "name hint" for pprinting
          | App Term Term
          | T
          | F
          | IfThenElse Term Term Term
          | Z
          | Succ Term
          | Pred Term
          | IsZero Term
          deriving Show

data NamedTerm = NVar String
               | NAbs String NamedTerm
               | NApp NamedTerm NamedTerm
               | NT
               | NF
               | NIfThenElse NamedTerm NamedTerm NamedTerm
               | NZ
               | NSucc NamedTerm
               | NPred NamedTerm
               | NIsZero NamedTerm
               deriving Show

isValue :: Term -> Bool
isValue (Abs _ _) = True
isValue T = True
isValue F = True
isValue Z = True
isValue (Succ x) = isNumeric x
isValue _ = False

isNumeric :: Term -> Bool
isNumeric Z = True
isNumeric (Succ x) = isNumeric x
isNumeric _ = False

subst :: Int -> Term -> Term -> Term
subst x what (Var n)
  | x == n = what
  | otherwise = (Var n)
subst x what (Abs t n) = Abs (subst (x + 1) (shift 1 what) t) n
subst x what (App t1 t2) = App (subst x what t1) (subst x what t2)
subst x what (IfThenElse t1 t2 t3) =
  IfThenElse (subst x what t1) (subst x what t2) (subst x what t3)
subst x what (Succ t) = Succ (subst x what t)
subst x what (Pred t) = Pred (subst x what t)
subst x what (IsZero t) = IsZero (subst x what t)
subst _ _ T = T
subst _ _ F = F
subst _ _ Z = Z

shift :: Int -> Term -> Term
shift = shift' 0 where
  shift' cutoff s (Var n)
    | n < cutoff = Var n
    | otherwise = Var (n + s)
  shift' cutoff s (Abs t n) = Abs (shift' (cutoff + 1) s t) n
  shift' cutoff s (App t1 t2) = App (shift' cutoff s t1) (shift' cutoff s t2)
  shift' cutoff s (IfThenElse t1 t2 t3) =
    IfThenElse (shift' cutoff s t1) (shift' cutoff s t2) (shift' cutoff s t3)
  shift' cutoff s (Succ t) = Succ (shift' cutoff s t)
  shift' cutoff s (Pred t) = Pred (shift' cutoff s t)
  shift' cutoff s (IsZero t) = IsZero (shift' cutoff s t)
  shift' _ _ T = T
  shift' _ _ F = F
  shift' _ _ Z = Z

eval :: Term -> Term
eval v
  | isValue v = v
eval a@(App t1 t2) = case eval t1 of
  Abs t n -> eval $ shift (-1) $ subst 0 (shift 1 (eval t2)) t
  _ -> a
eval ite@(IfThenElse b t f) = case eval b of
  T -> eval t
  F -> eval f
  _ -> ite
eval sx@(Succ x) = let x' = eval x in
  if isNumeric x' then (Succ x') else sx
eval px@(Pred x) = case eval x of
  Z -> Z
  (Succ x)
    | isNumeric x -> x
  _ -> px
eval izx@(IsZero x) = case eval x of
  Z -> T
  (Succ x)
    | isNumeric x -> F
  _ -> izx
eval x = x

evalNamed :: NamedTerm -> NamedTerm
evalNamed t = let (t', ctxt) = removeNames t in restoreNames (eval t') ctxt

freeVariables :: NamedTerm -> S.Set String
freeVariables = go S.empty where
  go bound (NVar x) = if S.member x bound then S.empty else S.singleton x
  go bound (NAbs x t) = go (S.insert x bound) t
  go bound (NApp t1 t2) = go bound t1 `S.union` go bound t2
  go bound (NIfThenElse t1 t2 t3) =
    go bound t1 `S.union` go bound t2 `S.union` go bound t3
  go bound (NSucc t) = go bound t
  go bound (NPred t) = go bound t
  go bound (NIsZero t) = go bound t
  go bound NT = S.empty
  go bound NF = S.empty
  go bound NZ = S.empty

removeNames :: NamedTerm -> (Term, [String])
removeNames t = (fromMaybe (error "impossible") (go t ctxt), ctxt) where
  ctxt = S.toList $ freeVariables t
  go (NVar n) ctxt = Var <$> elemIndex n ctxt
  go (NAbs n t) ctxt = Abs <$> go t (n:ctxt) <*> pure n
  go (NApp t1 t2) ctxt = App <$> go t1 ctxt <*> go t2 ctxt
  go (NIfThenElse t1 t2 t3) ctxt =
    IfThenElse <$> go t1 ctxt <*> go t2 ctxt <*> go t3 ctxt
  go (NSucc t) ctxt = Succ <$> go t ctxt
  go (NPred t) ctxt = Pred <$> go t ctxt
  go (NIsZero t) ctxt = IsZero <$> go t ctxt
  go NT _ = pure T
  go NF _ = pure F
  go NZ _ = pure Z

restoreNames :: Term -> [String] -> NamedTerm
restoreNames (Var n) ctxt = NVar $ ctxt !! n
restoreNames (Abs t n) ctxt = NAbs n' (restoreNames t (n':ctxt)) where
  n' = head $ filter (not . (`elem` ctxt)) (iterate (++ "'") n)
restoreNames (App t1 t2) ctxt =
  NApp (restoreNames t1 ctxt) (restoreNames t2 ctxt)
restoreNames (IfThenElse t1 t2 t3) ctxt = NIfThenElse
  (restoreNames t1 ctxt) (restoreNames t2 ctxt) (restoreNames t3 ctxt)
restoreNames (Succ t) ctxt = NSucc $ restoreNames t ctxt
restoreNames (Pred t) ctxt = NPred $ restoreNames t ctxt
restoreNames (IsZero t) ctxt = NIsZero $ restoreNames t ctxt
restoreNames T _ = NT
restoreNames F _ = NF
restoreNames Z _ = NZ
