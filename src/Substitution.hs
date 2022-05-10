{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Substitution where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types
import Context

newtype Subst = Subst (Map.Map TVar Type)
    deriving (Show, Eq, Ord, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst Map.empty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

class Substitutable a where
    -- | apply substitution over a
    apply :: Subst -> a -> a
    -- | get the free type variables
    ftv   :: a -> Set.Set TVar


instance Substitutable Type where
    apply _ (TCon a)        = TCon a
    apply (Subst s) t@(TVar a)      = Map.findWithDefault t a s
    apply s (t1 `TArr` t2)  = apply s t1 `TArr` apply s t2

    ftv TCon{}         = Set.empty
    ftv (TVar a)       = Set.singleton a
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as $ apply (Subst s') t
                            where s' = foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env


occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a = Set.member a . ftv