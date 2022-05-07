module Lib
    ( someFunc
    ) where
import Inference
import Syntax
import Context (emptyTyenv)

someFunc :: IO ()
someFunc = print $ runInfer (infer emptyTyenv (Op Add (Lit $ LInt 3) (Lit (LInt 3))))
