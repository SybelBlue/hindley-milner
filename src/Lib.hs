module Lib
    ( inferFresh
    ) where
import Inference
import Syntax
import qualified Context

inferFresh :: [Decl] -> Either TypeError Context.Env
inferFresh = inferTop Context.empty
