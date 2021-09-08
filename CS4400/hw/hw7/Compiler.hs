{- |
Module      :  Compiler
Description :  Compiler from protoScheme into Pure Lambda Calculus (PLC).
Copyright   :  (c) <your names>

Maintainer  : <your emails>
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
import Parser (parseSExpression)
import qualified SExpression as S

import SimpleTestsColor (test, testSection)

-- |Compile a protoScheme expression into PLC
compile :: Expr -> Maybe L.Lambda
compile (Integer i) = Just (toNumeral i)
compile (Boolean b) = Just (toChurchBool b)
compile (Var v) = Just (L.Var v)
compile (Add e1 e2) = compileBinaryOp cplus e1 e2
compile (Mul e1 e2) = compileBinaryOp ctimes e1 e2
compile (Sub e1 e2) = compileBinaryOp cminus e1 e2
compile (Let v e1 e2) = do
  c1 <- (compile e1)
  c2 <- (compile e2)
  return ((L.Lam v c2) `L.App` c1)
compile (If cn e1 e2) = do
  cnd <- (compile cn)
  c1 <- (compile e1)
  c2 <- (compile e2)
  return (cifthen `L.App` cnd `L.App` c1 `L.App` c2)
compile (And e1 e2) = compileBinaryOp cand e1 e2
compile (Or e1 e2) = compileBinaryOp cor e1 e2
compile (Not e) = do
  c <- (compile e)
  return (cnot `L.App` c)
compile (GreaterThan e1 e2) = compileBinaryOp cgt e1 e2
compile (LessThan e1 e2) = compileBinaryOp clt e1 e2
compile (Equals e1 e2) = compileBinaryOp ceq e1 e2
compile (Call fn e []) = do
  c <- (compile e)
  return (L.App (L.Var fn) c)
compile (Call fn e (e1:es)) = do
  a <- (compile (Call fn e es))
  c1 <- (compile e1)
  return (L.App a c1)
compile _ = Nothing

-- |Compile a binary protoScheme operation into PLC
compileBinaryOp :: L.Lambda -> Expr -> Expr -> Maybe L.Lambda
compileBinaryOp fn e1 e2 = do
  c1 <- (compile e1)
  c2 <- (compile e2)
  return (fn `L.App` c1 `L.App` c2)

-- |Compile the given protoScheme program into PLC
compileProgram :: Program -> Maybe L.Lambda
compileProgram (Program [] e) = (compile e)
compileProgram (Program (df:defs) e) = do
  cp <- compileProgram (Program defs e)
  cdf <- compileDef df cp
  return cdf

compileDef :: GlobalDef -> L.Lambda -> Maybe L.Lambda
compileDef (Define v ex) cp = do
  c <- (compile ex)
  return ((L.Lam v cp) `L.App` (normalize c))
compileDef (Defun v vs e) cp = do
  c <- (compile e)
  return ((L.Lam v cp) `L.App` (cfix `L.App` (lam (v:vs) (normalize c))))

-- |Generate the source code of a program calculating the factorial of the
-- given number
factorialProgram :: Integer -> String
factorialProgram number =
  "((defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact " ++ show number ++ "))"

-- |Generate Lambda expression for calculating the factorial of a given number
-- NOTE: this is to get rid of the Maybe L.Lambda that comes from compileProgram
factorial :: Integer -> L.Lambda
factorial n =
  case parseSExpression (factorialProgram n) of
    Just (S.List se) ->
      case compileProgram (programFromSExpression se) of
        Just p -> p
        Nothing -> (L.Var "None")
    Just e ->
      case compileProgram (programFromSExpression [e]) of
        Just p -> p
        Nothing -> (L.Var "None")
    Nothing -> (L.Var "None")

test_factorial = do
  -- NOTE: Takes a long time to execute
  test "(fact 3)"
    (fromNumeral $ normalize $ factorial 3)
    (Just 6)
  test "(fact 0)"
    (fromNumeral $ normalize $ factorial 0)
    (Just 1)

countdownTo1Program = 
    "((defun down (n)\
    \   (if (= n 1)\
    \       n\
    \       (down (- n 1))))\
    \ (down 5))"

allTests = do
    test_factorial

