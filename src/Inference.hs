module Inference where

import Control.Exception (TypeError)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (State)
import Data.Unique (Unique)

type Infer a = ExceptT TypeError (State Unique) a