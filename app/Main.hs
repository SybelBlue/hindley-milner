module Main ( main ) where

import Syntax
import Lib (inferFresh)
import Data.Map.Strict ((!))
import Context

instance Num Expr where
    (+) = Op Add
    (*) = Op Mul
    (-) = Op Sub
    abs e = If (Op Les e 0) (-e) e
    fromInteger = Lit . LInt
    signum e = If (Op Les e 0) (-1) $ If (Op Eql e 0) e 1

(\>) ps body = foldr Lam body ps
infixr 0 \>

decls =
    [ ("addTwo", ["x"] \> 2 + Var "x")
    , ("id", ["x"] \> Var "x")
    , ("addTwo2", Let "y" 2 $ ["x"] \> Var "x" + Var "y")
    , ("fib", Fix $ ["fib", "n"] \> App (Var "fib") (Var "n" - 1) + App (Var "fib") (Var "n" - 2))
    ]

main :: IO ()
main = case inferFresh decls of
  Left te -> do
      putStrLn "TypeError"
      print te
  Right (TypeEnv env) ->
      mapM_ (putStrLn . (\n -> n ++ " :: " ++ show (env ! n)) . fst) decls
