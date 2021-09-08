{- |
Module : Repl
Description : REPL for protoScheme. 
Copyright : (c) Ferd, 2020
Maintainer : f.vesely@northeastern.edu

-}
module Repl where

import Parser 
import Eval
import Syntax
import Maps (empty, keys)
import Result
import qualified SExpression as S

import System.IO (isEOF)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            putStrLn "protoScheme Version 1e-10"
            replLoop empty
            putStrLn "Bye bye."
            return ()
        else do
            file <- fromFile $ head args
            res <- toIO $ execute file
            putStrLn res
  where 
      execute inp = do
          program <- inp
          v <- runProgram program
          return $ S.toString v

-- |The main REPL loop
replLoop :: Env -> IO Env
replLoop env = do
    putStr "> "
    eof <- isEOF
    unless eof $ do
        line <- getLine
        unless (line == ":q") $ do
            env' <- processLine env line
            replLoop env'
  where
    unless b a = if b then return env
                      else a


-- |Process the given line
processLine :: Env -> String -> IO Env
processLine envs line 
    | opens line <= 0 = case parseSExpression line of
        Just sexpr -> parseAndEval envs sexpr
        Nothing -> do 
            putStrLn "Parse error. Try again."
            return envs
    | otherwise = do 
        putStr "  "
        more <- getLine
        processLine envs $ line ++ "\n" ++ more

opens :: String -> Int
opens str = o - c
  where
    (o, c) = foldl count (0, 0) str
    count (o, c) ch | ch `elem` "[(" = (o + 1, c)
                    | ch `elem` "])" = (o, c + 1)
                    | otherwise = (o, c)


-- |Parse the given s-expression
parseAndEval :: Env -> S.Expr -> IO Env
parseAndEval genv sexpr = case globalFromSExpr sexpr of
    Success global -> 
        unlessDefined global $ evalGlobal' global
    Failure _ -> do 
        eval' $ fromSExpression sexpr
        return genv
  where
    -- | Guard to check if a name is already used, printing an error message
    -- and doing nothing if it is.
    unlessDefined :: GlobalDef -> IO Env -> IO Env
    unlessDefined (Define x _) _ | x `elem` keys genv = do
        putStrLn $ "Variable " ++ x ++ " is already defined."
        return genv
    unlessDefined _ a = a
               

    -- | Evaluate a given global definition.
    evalGlobal' :: GlobalDef -> IO Env
    evalGlobal' global = case evalGlobal genv global of
        Success genv' -> do
            putStrLn $ describeGlobal global ++ " defined."
            return genv'
        Failure s -> do 
            putStrLn $ "Failed to evaluate definition: " ++ s ++ ". Try again."
            return genv

    -- | Evaluate the given expression with the current globals
    eval' :: Maybe Expr -> IO ()
    eval' (Just e) = case eval base genv e of
        Success v -> putStrLn $ S.toString $ valueToSExpression v
        Failure s -> putStrLn $ "Evaluation error: " ++ s ++ ". Try again."
    eval' Nothing = putStrLn "Parse error. Try again"

    -- |Describe the given global definition
    describeGlobal :: GlobalDef -> String
    describeGlobal (Define x _) = "Variable " ++ x


