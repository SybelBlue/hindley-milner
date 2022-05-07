module Inference where

import qualified Data.Map.Strict as Map

import Context
import Substitution
import Types 

import Control.Exception
import Control.Monad.Except
import Control.Monad.State


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