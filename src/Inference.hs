{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Inference where

import qualified Data.Map.Strict as Map

import Context
import Substitution
import Types 

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set
import Syntax
import Data.List (nub)
import Control.Monad.RWS
import Control.Monad.Identity (Identity)

data TypeError 
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable String
    deriving Show

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = StateT Unifier (ExceptT TypeError Identity) a

-- ReaderWriterStateTransformer a
--    read:  TypeEnv 
--    write: [Constraint]
--    state: InferState
--    trans: Except TypeError
--    out:   a
type Infer a = (RWST
                    TypeEnv
                    [Constraint]
                    InferState
                    (Except TypeError)
                    a)

newtype InferState = InferState { count :: Int }

uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTyenv (apply sub ty)
    
normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

runInfer :: Infer Type -> Either TypeError Scheme
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

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

lookupEnv :: Var -> Infer Type
lookupEnv x = do
    (TypeEnv env) <- ask
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable (show x)
        Just s  -> instantiate s

infer :: Expr -> Infer Type
infer ex = case ex of
    Lit (LInt _)  -> return typeInt
    Lit (LBool _) -> return typeBool

    Var x -> lookupEnv x

    Lam x e -> do
        tv <- fresh
        t <- inEnv (x, Forall [] tv) (infer e)
        return (tv `TArr` t)

    App e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh 
        uni t1 (t2 `TArr` tv)
        return tv
    
    Let x e1 e2 -> do
        env <- ask
        t1 <- infer e1
        let sc = generalize env t1
        inEnv (x, sc) (infer e2)

    Fix e1 -> do
        t1 <- infer e1
        tv <- fresh
        uni (tv `TArr` tv) t1
        return tv
    
    Op op e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh
        let u1 = t1 `TArr` (t2 `TArr` tv)
            u2 = ops op
        uni u1 u2
        return tv

    If cond tr fl -> do
        t1 <- infer cond
        t2 <- infer tr
        t3 <- infer fl
        uni t1 typeBool
        uni t2 t3
        return t2
    
