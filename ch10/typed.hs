-- the typed lambda calculus, using de Bruijn indices,
--   with a big-step evaluator, E N R I C H E D with booleans and nats

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import qualified Data.Set as S

data Ty = TBool
        | TNat
        | TFn Ty Ty
        deriving (Show, Eq)

data Term = Var Int
          | Abs Term Ty String -- the string is a "name hint" for pprinting
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
               | NAbs String Ty NamedTerm
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
isValue (Abs _ _ _) = True
isValue T = True
isValue F = True
isValue Z = True
isValue (Succ x) = isNumeric x
isValue _ = False

isNumeric :: Term -> Bool
isNumeric Z = True
isNumeric (Succ x) = isNumeric x
isNumeric _ = False

typeOf :: [Ty] -> Term -> Either String Ty
typeOf _ T = Right TBool
typeOf _ F = Right TBool
typeOf c (IfThenElse cond ifT elseT) = do
  condTy <- typeOf c cond
  case condTy of
    TBool -> do
      ifTy <- typeOf c ifT
      elseTy <- typeOf c elseT
      if ifTy == elseTy
        then Right ifTy
        else Left "branches of IfThenElse must have matching types"
    _ -> Left "condition of IfThenElse must be a boolean"

typeOf _ Z = Right TNat
typeOf c (Succ n) = do
  nTy <- typeOf c n
  case nTy of
    TNat -> Right TNat
    _ -> Left "argument to Succ must be a natural"
typeOf c (Pred n) = do
  nTy <- typeOf c n
  case nTy of
    TNat -> Right TNat
    _ -> Left "argument to Pred must be a natural"
typeOf c (IsZero n) = do
  nTy <- typeOf c n
  case nTy of
    TNat -> Right TBool
    _ -> Left "argument to IsZero must be a natural"

typeOf c (Var i) = Right $ c !! i
typeOf c (Abs t ty _) = do
  resTy <- typeOf (ty:c) t
  Right $ TFn ty resTy
typeOf c (App t1 t2) = do
  fnTy <- typeOf c t1
  case fnTy of
    TFn fromTy toTy -> do
      argTy <- typeOf c t2
      if fromTy == argTy
        then Right toTy
        else Left $
          "abstraction expected " ++ show fromTy ++ ", got " ++ show argTy
    _ -> Left "attempted application of non-function"

subst :: Int -> Term -> Term -> Term
subst x what (Var n)
  | x == n = what
  | otherwise = (Var n)
subst x what (Abs t ty n) = Abs (subst (x + 1) (shift 1 what) t) ty n
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
  shift' cutoff s (Abs t ty n) = Abs (shift' (cutoff + 1) s t) ty n
  shift' cutoff s (App t1 t2) = App (shift' cutoff s t1) (shift' cutoff s t2)
  shift' cutoff s (IfThenElse t1 t2 t3) =
    IfThenElse (shift' cutoff s t1) (shift' cutoff s t2) (shift' cutoff s t3)
  shift' cutoff s (Succ t) = Succ (shift' cutoff s t)
  shift' cutoff s (Pred t) = Pred (shift' cutoff s t)
  shift' cutoff s (IsZero t) = IsZero (shift' cutoff s t)
  shift' _ _ T = T
  shift' _ _ F = F
  shift' _ _ Z = Z

eval :: Term -> Either String Term
eval t = do
  _ <- typeOf [] t
  Right $ eval' t
  where
    eval' v
      | isValue v = v
    eval' a@(App t1 t2) = case eval' t1 of
      Abs t _ n -> eval' $ shift (-1) $ subst 0 (shift 1 (eval' t2)) t
      _ -> a
    eval' ite@(IfThenElse b t f) = case eval' b of
      T -> eval' t
      F -> eval' f
      _ -> ite
    eval' sx@(Succ x) = let x' = eval' x in
      if isNumeric x' then (Succ x') else sx
    eval' px@(Pred x) = case eval' x of
      Z -> Z
      (Succ x)
        | isNumeric x -> x
      _ -> px
    eval' izx@(IsZero x) = case eval' x of
      Z -> T
      (Succ x)
        | isNumeric x -> F
      _ -> izx
    eval' x = x

evalNamed :: NamedTerm -> Either String NamedTerm
evalNamed t = let (t', ctxt) = removeNames t in
              flip restoreNames ctxt <$> eval t'

freeVariables :: NamedTerm -> S.Set String
freeVariables = go S.empty where
  go bound (NVar x) = if S.member x bound then S.empty else S.singleton x
  go bound (NAbs x _ t) = go (S.insert x bound) t
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
  go (NAbs n ty t) ctxt = Abs <$> go t (n:ctxt) <*> pure ty <*> pure n
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
restoreNames (Abs t ty n) ctxt = NAbs n' ty (restoreNames t (n':ctxt)) where
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
