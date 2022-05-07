module Substitution where

import qualified Data.Map.Strict as Map
import Types (TVar, Type)

type Subst = Map.Map TVar Type