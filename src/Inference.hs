module Inference where

import qualified Data.Map.Strict as Map

import Context
import Substitution
import Types 

import Control.Monad.Except
import Control.Monad.State

data TypeError 
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable String

newtype Unique = Unique { count :: Int }

initUnique = Unique { count = 0 }

type Infer a = ExceptT TypeError (State Unique) a

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTyenv (apply sub ty)

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
    Left err -> Left err
    Right res -> Right $ closeOver res

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

unify :: Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t

unify (TCon a) (TCon b) | a == b = return nullSubst

unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a      = return nullSubst
         | occursCheck a t  = throwError $ InfiniteType a t
         | otherwise        = return $ Map.singleton a t