{- |
Module      :  Compiler
Description :  Compiler from protoScheme into Pure Lambda Calculus (PLC).
Copyright   :  (c) Ferd

Maintainer  : f.vesely@northeastern.edu
-}

module Compiler 
    ( compile
    , compileProgram
    , factorialProgram
    , Compiler.allTests
    ) where


import Syntax
import Church
import qualified Lambda as L
import Reduce (normalize)
import Parser

import SimpleTestsColor (test, testSection)

import Control.Monad (mapM)
import Data.Foldable (foldrM)

-- |Compile a protoScheme expression into PLC
compile :: Expr -> Maybe L.Lambda
compile (Value v) = compileValue v
  where
    compileValue (Integer i) = Just $ toNumeral i
    compileValue (Boolean b) = Just $ toChurchBool b
    compileValue _ = Nothing
compile (Var x) = Just $ L.Var x
compile (Add e1 e2) = compApp cplus [e1, e2]
compile (Sub e1 e2) = compApp cminus [e1, e2]
compile (Mul e1 e2) = compApp ctimes [e1, e2]
compile (Let x e1 e2) = L.App <$> (L.Lam x <$> compile e2) <*> compile e1 
compile (If e1 e2 e3) = compApp cifthen [e1, e2, e3]
compile (And e1 e2) = compApp cand [e1, e2]
compile (Or e1 e2) = compApp cor [e1, e2]
compile (Not e) = compApp cnot [e]
compile (Lt e1 e2) = compApp clt [e1, e2]
compile (Gt e1 e2) = compApp cgt [e1, e2]
compile (Eq e1 e2) = compApp ceq [e1, e2]
compile (Pair e1 e2) = compApp cpair [e1, e2]
compile (PLeft e) = compApp cleft [e]
compile (PRight e) = compApp cright [e]
compile (Call f es) = compApp (L.Var f) es

-- | Helper: compile the given protoScheme expressions and apply the given 
-- lambda expression to them
compApp :: L.Lambda -> [Expr] -> Maybe L.Lambda
compApp op es = do
  es' <- mapM compile es
  return $ foldl L.App op es'

-- |Compile the given protoScheme program into PLC
compileProgram :: Program -> Maybe L.Lambda
compileProgram (Program globals expr) =
    compile expr >>= defns
  where
    defns :: L.Lambda -> Maybe L.Lambda
    defns expr = foldrM glob expr globals
    glob :: GlobalDef -> L.Lambda -> Maybe L.Lambda
    glob (Define x e) body = L.App (L.Lam x body) <$> compile e
    glob (Defun f xs e) body = 
        let fn = L.App cfix <$> lam (f : xs) <$> normalize <$> compile e
        in L.App (L.Lam f body) <$> fn

-- |Generate the source code of a program calculating the factorial of the
-- given number
factorialProgram :: Integer -> String
factorialProgram number = 
  "((defun fact (n)\
  \  (if (= n 0)\
  \      1\
  \      (* n (fact (- n 1)))))\
  \ (fact " ++ show number ++ "))" 

test_factorial = do
    test "factorial 1"
        (runFactorial 1)
        (Just 1)
    test "factorial 2"
        (runFactorial 2)
        (Just 2)
    test "factorial 3"
        (runFactorial 3)
        (Just 6)
{-    test "factorial 4"
        (runFactorial 4)
        (Just 24)-}
  where
    runFactorial n = do
        sexpr <- parseSExpression $ factorialProgram n
        progr <- programFromSExpression sexpr
        compiled <- compileProgram progr
        fromNumeral $ normalize compiled

countdownTo1Program = 
    "((defun down (n)\
    \   (if (= n 1)\
    \       n\
    \       (down (- n 1))))\
    \ (down 5))"

allTests = do
    test_factorial

