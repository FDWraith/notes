{- |
Module      :  Assignment05
Description :  Assignment 5 submission for CS 4400.
Copyright   :  (c) Jack, 2020
                   Ferd, 2020

Maintainer  :  gelinas.j@northeastern.edu
               f.vesely@northeastern.edu
-}

module Assignment05 where

import Syntax
import Eval

import SimpleTestsColor (testSection, beginTests, endTests)

main = do
    beginTests
    Syntax.allTests
    Eval.allTests
    endTests


