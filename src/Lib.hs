module Lib
    ( someFunc
    ) where
import Inference (runInfer, infer)
import Context (emptyTyenv)
import Syntax

someFunc :: IO ()
someFunc = print $ runInfer (infer emptyTyenv (Op Add (Var "a") (Lit (LInt 3))))
