{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import Data.Proxy
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
  VFn :: (Term as (TFn a b)) -> Value (TFn a b)

data Ctxt :: [Ty] -> * where
  CNil :: Ctxt '[]
  CCons :: (Value a) -> Ctxt as -> Ctxt (a ': as)

lookup :: Idx as a -> Ctxt as -> Value a
lookup IxZero (CCons v _) = v
lookup (IxSucc i) (CCons _ vs) = lookup i vs

eval :: Term '[] a -> Value a
eval = eval' CNil where
  eval' :: Ctxt as -> Term as a -> Value a
  eval' c (Var i) = lookup i c
  eval' _ f@(Abs _ _ _) = VFn f
  eval' c (App t1 t2) = case eval' c t1 of
    (VFn (Abs t1' _ _)) -> undefined
  eval' _ T = VBool True
  eval' _ F = VBool False
  eval' c (IfThenElse b t f) = case eval' c b of
    VBool True -> eval' c t
    VBool False -> eval' c f
  eval' _ Z = VNat 0
  eval' c (Succ n) = case eval' c n of
    VNat n' -> VNat $ n' + 1
  eval' c (Prev n) = case eval' c n of
    VNat n' -> VNat $ n' - 1
  eval' c (IsZero n) = case eval' c n of
    VNat n' -> VBool $ n' == 0

-- need shift and subst

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
