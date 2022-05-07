module Substitution where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types
import Context (TypeEnv(..))

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    -- | apply substitution over a
    apply :: Subst -> a -> a
    -- | get the free type variables
    ftv   :: a -> Set.Set TVar


instance Substitutable Type where
    apply _ (TCon a)        = TCon a
    apply s t@(TVar a)      = Map.findWithDefault t a s
    apply s (t1 `TArr` t2)  = apply s t1 `TArr` apply s t2

    ftv TCon{}         = Set.empty
    ftv (TVar a)       = Set.singleton a
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env


occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a = Set.member a . ftv