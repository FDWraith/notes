{- |
Module      :  Assignment06
Description :  Assignment 6 submission for CS 4400.
Copyright   :  (c) Kevin Zhang

Maintainer  :  zhang.kevi@northeastern.edu
-}

module Assignment06 where

import Syntax
import Eval
import qualified SExpression as S

import Repl

{-
main :: IO()
main = do
  test_eval
  test_subst
  test_runSExpression
  test_toSExpression
  test_fromSExpression
  test_condsToSExpression
  test_fromConditionals
  test_valueToSExpression
  test_eval_if
  test_globalDefFromSExpression
  test_programFromSExpression
  test_substValues
  test_evalProgram
-}

main :: IO()
main = do
  loop []
  return ()
