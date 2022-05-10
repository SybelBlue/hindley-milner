module Context where

import qualified Data.Map.Strict as Map
import Syntax (Name)
import Types (Scheme)

newtype Env = TypeEnv { types :: Map.Map Name Scheme } deriving Show

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Name, Scheme) -> Env
extend (TypeEnv env) (x, s) = TypeEnv (Map.insert x s env)

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Name, Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }