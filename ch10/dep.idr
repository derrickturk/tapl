import Data.Vect

%default total

-- gaaaaaah some built-in Var type blows us up, use a dumb name

data Term : Vect n Type -> Type -> Type where
  Vxr : {ts : Vect n Type} -> (i : Fin n) -> Term ts (index i ts)
  Abs : (a : Type) -> Term (a :: ts) b -> Term ts (a -> b)
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

index : {ts : Vect n Type} -> (i : Fin n) -> Env ts -> index i ts
index FZ (x :: _) = x
index (FS k) (_ :: xs) = index k xs

eval : Env ts -> Term ts a -> a
eval e (Vxr i) = index i e
eval e (Abs t body) = \x => eval (x :: e) body
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

example : Term [] Nat
example = App (App (Abs Bool (IfThenElse (Vxr FZ) (Abs Nat (Succ (Vxr FZ)))
                                                  (Abs Nat (Prev (Vxr FZ)))))
                   T)
              (Succ (Succ Z))
