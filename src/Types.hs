module Types where

import Syntax

import qualified Data.Map.Strict as Map
newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Type
    = TVar TVar
    | TCon String
    | TArr Type Type
    deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt = TCon "Int"
typeBool = TCon "Bool"

data Scheme = Forall [TVar] Type deriving Show

ops :: Binop -> Type
ops Add = typeInt `TArr` typeInt `TArr` typeInt
ops Mul = typeInt `TArr` typeInt `TArr` typeInt
ops Sub = typeInt `TArr` typeInt `TArr` typeInt
ops Eql = typeInt `TArr` typeInt `TArr` typeBool