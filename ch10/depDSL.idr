import Data.Vect

%language DSLNotation
%default total

-- had to tweak this one to get inference to work well enough for
--   use in Idris's DSL notation

data HasType : (i : Fin n) -> Vect n Type -> Type -> Type where
  TypeHere : HasType FZ (a :: ts) a
  TypeThere : HasType j ts t -> HasType (FS j) (any :: ts) t

data Term : Vect n Type -> Type -> Type where
  Var : HasType i ts t -> Term ts t
  Abs : Term (a :: ts) b -> Term ts (a -> b)
  App : Term ts (a -> b) -> Term ts a -> Term ts b
  T : Term ts Bool
  F : Term ts Bool
  IfThenElse : Term ts Bool -> Term ts a -> Term ts a -> Term ts a
  Z : Term ts Nat
  Prev : Term ts Nat -> Term ts Nat
  Succ : Term ts Nat -> Term ts Nat
  IsZero : Term ts Nat -> Term ts Bool

data Env : Vect n Type -> Type where
  Nil : Env []
  (::) : a -> Env ts -> Env (a :: ts)

lookup : Env ts -> HasType i ts t -> t
lookup (x :: _) TypeHere = x
lookup (_ :: xs) (TypeThere prf) = lookup xs prf

eval : Env ts -> Term ts a -> a
eval e (Var i) = lookup e i
eval e (Abs body) = \x => eval (x :: e) body
eval e (App f x) = eval e f (eval e x)
eval _ T = True
eval _ F = False
eval e (IfThenElse b x y) = if eval e b
  then eval e x
  else eval e y
eval _ Z = Z
eval e (Prev n) = case eval e n of
  Z => Z
  (S k) => k
eval e (Succ n) = succ (eval e n)
eval e (IsZero n) = case eval e n of
  Z => True
  (S k) => False

mkLambda : TTName -> Term (a :: ts) b -> Term ts (a -> b)
mkLambda _ t = Abs t

dsl stlc
  variable = Var
  index_first = TypeHere
  index_next = TypeThere
  lambda =  mkLambda

pure : Term ts a -> Term ts a
pure = id

(<*>) : Term ts (a -> b) -> Term ts a -> Term ts b
(<*>) = App

syntax IF [c] THEN [x] ELSE [y] = IfThenElse c x y

example : Term ts (Bool -> Nat -> Nat)
example = stlc (\c, n => IF c THEN Succ n ELSE Prev n)

idiomExample : Term ts Nat
idiomExample = stlc [| (\x => Succ x) Z |]
