{- |
Module      :  Assignment08
Description :  Assignment 8 submission for CS 4400.
Copyright   :  (c) Kevin Zhang

Maintainer  :  zhang.kevi@northeastern.edu
-}

-- import Repl

import Syntax

import Eval

import Repl

import Maps

import Parser

import Result

import qualified SExpression as S

main :: IO ()
main = do
    putStrLn "protoScheme Version 1e-10"
    replLoop (empty, empty)
    putStrLn "Bye bye!"


sample :: IO ()
sample = do
  fname <- getLine
  ex <- fromFile fname
  case ex of
    Success sexpr -> print $ show $ programFromSExpression $ S.List sexpr
    _ -> print "failed"

allTests :: IO()
allTests = do
  test_eval
  test_fromSExpression
  test_toSExpression
