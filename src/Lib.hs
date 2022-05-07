module Lib
    ( inferTest
    ) where
import Inference
import Syntax
import Context (emptyTyenv)

inferTest :: IO ()
inferTest = print $ runInfer (infer emptyTyenv (Op Add (Lit $ LInt 3) (Lit (LInt 3))))
