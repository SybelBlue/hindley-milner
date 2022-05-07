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

data TypeError 
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable String
    deriving Show

newtype Unique = Unique { count :: Int }

initUnique = Unique { count = 0 }

type Infer a = ExceptT TypeError (State Unique) a

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

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = 
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable (show x)
        Just s  -> do 
            t <- instantiate s
            return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

    Var x -> lookupEnv env x

    Lam x e -> do
        -- by convention, lam param (x) is typed as concretely as possible.
        -- | (\f -> let g = f True in f 3) id
        -- > TypeMismatch `Bool` and `Int` in instance (f 3), f :: Bool -> Bool
        xtype <- fresh
        let env' = env `extend` (x, Forall [] xtype)
        (retsub, rettype) <- infer env' e
        return (retsub, apply retsub xtype `TArr` rettype)
    
    App fexpr pexpr -> do
        rtype <- fresh
        (fsub, ftype) <- infer env fexpr
        (psub, ptype) <- infer (apply fsub env) pexpr
        rsub          <- unify (apply psub ftype) (TArr ptype rtype)
        return (rsub `compose` psub `compose` fsub, apply rsub rtype)
    
    Let x xbind e -> do
        (bindsub, bindtype) <- infer env xbind
        -- by convention, let bindings are generalized as much as possible
        -- > let f x = x in let g = f True in f 3 -- f is Bool -> Bool and Nat -> Nat
        -- | 3 : Int
        let env'      = apply bindsub env
            bindtype' = generalize env' bindtype
            binding   = (x, bindtype')
        (rsub, rtype) <- infer (env' `extend` binding) e
        return (bindsub `compose` rsub, rtype)
    
    If cond tr fl -> do
        (csub, ctype) <- infer env cond
        (tsub, ttype) <- infer env tr
        (fsub, ftype) <- infer env fl
        s1 <- unify ctype typeBool
        s2 <- unify ttype ftype
        return (s2 `compose` s1 `compose` fsub `compose` tsub `compose` csub, apply s2 ttype)
    
    Fix e -> do
        (s1, t) <- infer env e
        -- fixpoint recurses on itself
        tv <- fresh
        s2 <- unify (TArr tv tv) t
        return (s2, apply s1 tv)
    
    Op op l r -> do
        (lsub, ltype) <- infer env l
        (rsub, rtype) <- infer env r
        rettype <- fresh
        retsub <- unify (ltype `TArr` rtype `TArr` rettype) (ops op)
        return (lsub `compose` rsub `compose` retsub, apply retsub rettype)
    
    Lit (LInt _)  -> return (nullSubst, typeInt)
    Lit (LBool _) -> return (nullSubst, typeBool)