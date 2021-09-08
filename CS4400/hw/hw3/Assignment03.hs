{- |
Module      :  Assignment03
Description :  Assignment 3 submission for CS 4400.
Copyright   :  (c) Kevin Zhang

Maintainer  :  zhang.kevi@northeastern.edu
-}

module Assignment03 where

import Syntax
import Eval
import qualified SExpression as S

main :: IO()
main = do
  test_eval
  test_subst
  test_runSExpression
  test_toSExpression
  test_fromSExpression
  test_valueToSExpression
