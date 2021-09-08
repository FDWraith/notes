{- |
Module      :  Assignment09
Description :  Assignment 9 submission for CS 4400.
Copyright   :  (c) Ferd, 2020

Maintainer  :  f.vesely@northeastern.edu

-}

import qualified TypeCheck

import Parser
import Syntax
import Eval
import TypeCheck
import qualified Types as T
import qualified SExpression as S
import Result

import SimpleTestsColor (beginTests, endTests, testSection)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    process args
  where
    process :: [String] -> IO ()
    process [] = return ()
    process ("--type" : file : rest) = do
        result <- fromFile file 
        typ <- doProgram result typeOfProgramSExpr
        putStrLn $ S.toString typ
        process rest
    process ("--eval" : file : rest) = do
        result <- fromFile file
        val <- doProgram result runProgram
        putStrLn $ S.toString val
        process rest
    process ("--tests" : rest) = do
        putStrLn "Running tests..."
        Main.allTests
        process rest
    process args = do
        putStrLn $ "Unknown arguments: " ++ show args
        return ()

    doProgram :: Result [S.Expr] -> ([S.Expr] -> Result a) -> IO a
    doProgram r f = toIO $ r >>= f



allTests :: IO ()
allTests = do
  Syntax.allTests
  Eval.allTests
  TypeCheck.allTests
  

