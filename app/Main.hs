module Main ( main ) where

import Syntax
import Lib (inferFresh)
import Data.Map.Strict ((!))
import Context
import Control.Arrow

decls = 
    [ ("addTwo", Lam "x" $ Op Add (Lit (LInt 2)) (Var "x"))
    , ("id", Lam "x" $ Var "x")
    , ("addTwo2", Let "y" (Lit $ LInt 2) $ Lam "x" $ Op Add (Lit (LInt 2)) (Var "x"))
    , ("fib", Fix $ Lam "fib" $ Lam "n" $ Op Add (App (Var "fib") $  Op Sub (Var "n") $ Lit $ LInt 1) (App (Var "fib") $ Op Sub (Var "n") $ Lit $ LInt 2))
    ]

main :: IO ()
main = case inferFresh decls of
  Left te -> do
      putStrLn "TypeError"
      print te
  Right (TypeEnv env) ->
      mapM_ (putStrLn . (\n -> n ++ " :: " ++ show (env ! n)) . fst) decls
