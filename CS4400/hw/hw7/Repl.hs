{-|
Module      : Repl
Description : protoScheme REPL
Copyright   : (c) Kevin Zhang, 2020
Maintainer  : zhang.kevi@northeastern.edu

This module provides a REPL to run protoScheme programs

-}
module Repl where

import Eval

import Maps

import Syntax

import Parser

loop :: GlobalEnv -> IO GlobalEnv
loop ev = do
  putStr "> "
  line <- getLine
  processLine line

  where
    processLine ":quit" = do
      putStrLn "Exiting Repl"
      return []
      
    processLine line = do
      sexpr <- processSExpr line
      processGDOrExpr sexpr
      return []

    processSExpr line = do
      case parseSExpression line of
        Just v -> return $ Just v
        Nothing -> do
          putStrLn "Parse Error: try again."
          loop ev
          return Nothing

    processGDOrExpr ex = do
      case ex of
        Just e -> 
          case globalDefFromSExpression e of
            Just _ -> processGD e
            Nothing -> processExpr e
        Nothing -> do
          return []

    processGD sexpr = do
      case globalDefFromSExpression sexpr of
        Just (Define v ex) -> 
          case get ev v of
            Nothing -> do
              putStrLn $ "Variable " ++ v ++ " defined."
              loop (set ev v (Define v ex))
            _ -> do
              putStrLn $ "Error: variable " ++ v ++ " was already defined."
              loop ev
        Just (Defun f args ex) ->
          case get ev f of
            Nothing -> do
              putStrLn $ "Function " ++ f ++ " defined."
              loop (set ev f (Defun f args ex))
            _ -> do
              putStrLn $ "Error: function " ++ f ++ " was already defined."
              loop ev         
    
    processExpr sexpr = do
      case eval ev $ fromSExpression sexpr of
        Just v -> do
          putStrLn $ show $ valueToSExpression v
          loop ev
        Nothing -> do
          putStrLn $ "Evaluation Error: try again."
          loop ev
