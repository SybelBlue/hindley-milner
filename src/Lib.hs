module Lib
    ( inferTest
    ) where
import Inference
import Syntax
import qualified Context

inferTest :: IO ()
inferTest = print $ inferTop Context.empty
    [ 
    ]
