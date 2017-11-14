-- exercise 6.1.5

import Data.List (elemIndex)

type VarName = String

type NameCtxt = [String]

data NamedTerm = Var VarName
               | Abs VarName NamedTerm
               | App NamedTerm NamedTerm
               deriving Show

data DBTerm = DBVar Int
            | DBAbs DBTerm
            | DBApp DBTerm DBTerm
            deriving Show

removeNames :: NamedTerm -> NameCtxt -> Maybe DBTerm
removeNames (Var n) ctxt = DBVar <$> elemIndex n ctxt
removeNames (Abs n t) ctxt = DBAbs <$> removeNames t (n:ctxt)
removeNames (App t1 t2) ctxt =
  DBApp <$> removeNames t1 ctxt <*> removeNames t2 ctxt

restoreNames :: DBTerm -> NameCtxt -> NamedTerm
restoreNames (DBVar n) ctxt = Var $ ctxt !! n
restoreNames (DBAbs t) ctxt = Abs n (restoreNames t (n:ctxt)) where
  n = head $ filter (not . (`elem` ctxt)) vars
  vars = map return ['a'..'z'] ++ map (++ "'") vars
restoreNames (DBApp t1 t2) ctxt =
  App (restoreNames t1 ctxt) (restoreNames t2 ctxt)
