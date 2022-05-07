module Substitution where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types (TVar, Type)

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar