{- |
Module      :  Assignment06
Description :  Assignment 6 submission for CS 4400.
Copyright   :  (c) Ferd, 2020

Maintainer  :  f.vesely@northeastern.edu
-}

module Assignment06 where

import qualified Repl

import qualified Eval (allTests)
import qualified Syntax (allTests)
import SimpleTestsColor (beginTests, endTests, testSection)

main :: IO ()
main = Repl.main

allTests :: IO ()
allTests = do
    beginTests
    testSection "Syntax tests"
    Syntax.allTests
    testSection "Eval tests"
    Eval.allTests
    endTests

