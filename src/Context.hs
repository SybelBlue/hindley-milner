module Context where

import qualified Data.Map.Strict as Map
import Syntax (Var)
import Types (Scheme)

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)

emptyTyenv = TypeEnv Map.empty

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv (Map.insert x s env)