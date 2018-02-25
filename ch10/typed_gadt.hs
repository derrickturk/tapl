{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- with lots of help from https://github.com/goldfirere/glambda
-- frankly, I wouldn't have come up with the quantifier placement
--   on the De Bruijn bits by myself,
-- and I'm not sure the whole exercise was worth it

import Prelude hiding (lookup)

data Ty =
    TBool
  | TNat
  | TFn Ty Ty
  deriving (Show, Eq)

data STy :: Ty -> * where
  SBool :: STy TBool
  SNat :: STy TNat
  SFn :: (STy a) -> (STy b) -> STy (TFn a b)

data Idx :: [Ty] -> Ty -> * where
  IxZero :: Idx (a ': as) a
  IxSucc :: Idx as b -> Idx (a ': as) b

data Term :: [Ty] -> Ty -> * where
  Var :: Idx as a -> Term as a
  Abs :: Term (a ': as) b -> (STy a) -> String -> Term as (TFn a b)
  App :: Term as (TFn a b) -> Term as a -> Term as b
  T :: Term as TBool
  F :: Term as TBool
  IfThenElse :: Term as TBool -> Term as a -> Term as a -> Term as a
  Z :: Term as TNat
  Succ :: Term as TNat -> Term as TNat
  Prev :: Term as TNat -> Term as TNat
  IsZero :: Term as TNat -> Term as TBool

data Value :: Ty -> * where
  VBool :: Bool -> Value TBool
  VNat :: Int -> Value TNat
  VFn :: (Term '[] (TFn a b)) -> Value (TFn a b)

data Ctxt :: [Ty] -> * where
  CNil :: Ctxt '[]
  CCons :: (Value a) -> Ctxt as -> Ctxt (a ': as)

data CtxtLen :: [Ty] -> * where
  LZero :: CtxtLen '[]
  LSucc :: CtxtLen as -> CtxtLen (a ': as)

type family (as :: [Ty]) ++ (bs :: [Ty]) :: [Ty]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs = a ': (as ++ bs)
infixr 5 ++

lookup :: Idx as a -> Ctxt as -> Value a
lookup IxZero (CCons v _) = v
lookup (IxSucc i) (CCons _ vs) = lookup i vs

eval :: Term '[] a -> Value a
eval (Var i) = error "impossible case"
eval f@(Abs _ _ _) = VFn f
eval (App t1 t2) = case eval t1 of
  (VFn (Abs t1' _ _)) -> eval $ subst t2 t1'
eval T = VBool True
eval F = VBool False
eval (IfThenElse b t f) = case eval b of
  VBool True -> eval t
  VBool False -> eval f
eval Z = VNat 0
eval (Succ n) = case eval n of
  VNat n' -> VNat $ n' + 1
eval (Prev n) = case eval n of
  VNat 0 -> VNat 0
  VNat n' -> VNat $ n' - 1
eval (IsZero n) = case eval n of
  VNat n' -> VBool $ n' == 0

push :: forall as a r . Term as r -> Term (a ': as) r
push = shift LZero where
  -- shift a term (with CtxtLen bs as the shift cutoff)
  shift :: forall bs r .
           CtxtLen bs
        -> Term (bs ++ as) r
        -> Term (bs ++ a ': as) r
  shift c (Var i) = Var $ bump c i
  shift c (Abs t ty n) = Abs (shift (LSucc c) t) ty n
  shift c (App t1 t2) = App (shift c t1) (shift c t2)
  shift c (IfThenElse b t f) = IfThenElse (shift c b) (shift c t) (shift c f)
  shift c (Succ n) = Succ $ shift c n
  shift c (Prev n) = Prev $ shift c n
  shift c (IsZero n) = IsZero $ shift c n
  shift _ T = T
  shift _ F = F
  shift _ Z = Z

  bump :: forall bs r .
          CtxtLen bs
       -> Idx (bs ++ as) r
       -> Idx (bs ++ a ': as) r
  -- if cutoff is zero, bump by one
  bump LZero i = IxSucc i
  -- if cutoff > zero and bumpee is zero, don't bump
  bump (LSucc _) IxZero = IxZero
  -- otherwise, go by induction on both...
  bump (LSucc l) (IxSucc i) = IxSucc $ bump l i

subst :: forall as a r . Term as a -> Term (a ': as) r -> Term as r 
subst what = subst' LZero where
  subst' :: forall bs r .
            CtxtLen bs
         -> Term (bs ++ a ': as) r
         -> Term (bs ++ as) r
  subst' l (Var i) = substVar l i
  subst' l (Abs t ty n) = Abs (subst' (LSucc l) t) ty n
  subst' l (App t1 t2) = App (subst' l t1) (subst' l t2)
  subst' l (IfThenElse b t f) =
    IfThenElse (subst' l b) (subst' l t) (subst' l f)
  subst' l (Succ n) = Succ $ subst' l n
  subst' l (Prev n) = Prev $ subst' l n
  subst' l (IsZero n) = IsZero $ subst' l n
  subst' _ T = T
  subst' _ F = F
  subst' _ Z = Z

  substVar :: forall bs r .
              CtxtLen bs
           -> Idx (bs ++ a ': as) r
           -> Term (bs ++ as) r
  substVar LZero IxZero = what
  substVar LZero (IxSucc i) = Var i
  substVar (LSucc _) IxZero = Var IxZero
  substVar (LSucc l) (IxSucc i) = push $ substVar l i

-- Show instance for GADTs

instance Show (STy a) where
  show SBool = "SBool"
  show SNat = "SNat"
  show (SFn a b) = "SFn (" ++ show a ++ ") (" ++ show b ++ ")"

instance Show (Idx as a) where
  show IxZero = "IxZero"
  show (IxSucc i) = "IxSucc (" ++ show i ++ ")"

instance Show (Term as a) where
  show (Var i) = "Var (" ++ show i ++ ")"
  show (Abs t ty n) = "Abs (" ++ show t ++ ") (" ++ show ty ++ ") " ++ show n
  show (App f t) = "App (" ++ show f ++ ") (" ++ show t ++ ")"
  show T = "T"
  show F = "F"
  show (IfThenElse b t f) = "IfThenElse (" ++ show b ++ ") ("
    ++ show t ++ ") (" ++ show f ++ ")"
  show Z = "Z"
  show (Succ n) = "Succ (" ++ show n ++ ")"
  show (Prev n) = "Prev (" ++ show n ++ ")"
  show (IsZero n) = "IsZero (" ++ show n ++ ")"

instance Show (Value a) where
  show (VBool b) = "VBool " ++ show b 
  show (VNat n) = "VNat " ++ show n
  show (VFn f) = "VFn (" ++ show f ++ ")"

example :: Term '[] TBool
example = (App
    (Abs (App (Var IxZero) (Succ Z)) (SFn SNat SBool) "f")
    (Abs (IsZero (Var IxZero)) SNat "n"))

{--
example :: Term '[] TNat
example = (App
    (App (Abs (Abs (Var $ IxSucc IxZero) SNat "y") SNat "x") (Succ Z))
    (Succ (Succ Z)))
--}
