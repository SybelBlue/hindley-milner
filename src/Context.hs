module Context where

import qualified Data.Map.Strict as Map
import Syntax (Var)
import Types (Scheme)

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)