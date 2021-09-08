module Repl where

import Result
import Parser 
import Eval
import Syntax
import Maps (empty, keys)
import qualified SExpression as S

import qualified Control.Exception as E

import System.IO (isEOF)

type Bindings = (Env, GlobalEnv)

-- |The main REPL loop
replLoop :: Bindings -> IO Bindings
replLoop envs = do
    putStr "> "
    eof <- isEOF
    unless eof $ do
        line <- getLine
        unless (line `elem` [":q", ":quit"]) $ do
            envs' <- processLine envs line
            replLoop envs'
  where
    unless b a = if b then return envs
                      else a

-- |Process the given line
processLine :: Bindings -> String -> IO Bindings
processLine envs line = case parseSExpression line of
    Success sexpr -> parseAndEval envs sexpr
    Failure _ -> do 
        putStrLn "Parse error. Try again."
        return envs

-- |Parse the given s-expression
parseAndEval :: Bindings -> S.Expr -> IO Bindings
parseAndEval envs@(env, genv) sexpr = 
  case globalFromSExpr sexpr of
    Success global -> 
        unlessDefined global $ evalGlobal' global
    Failure _ -> do 
        eval' $ fromSExpression sexpr
        return envs
  where
    -- | Guard to check if a name is already used, printing an error message
    -- and doing nothing if it is.
    unlessDefined :: GlobalDef -> IO Bindings -> IO Bindings
    unlessDefined (Define x _) _ | x `elem` keys genv = do
        putStrLn $ "Variable " ++ x ++ " is already defined."
        return envs
    unlessDefined _ a = a               

    -- | Evaluate a given global definition.
    evalGlobal' :: GlobalDef -> IO Bindings
    evalGlobal' global = case evalGlobal genv global of
        Success genv' -> do
            putStrLn $ describeGlobal global ++ " defined."
            return (env, genv')
        Failure _ -> do 
            putStrLn "Failed to evaluate definition. Try again."
            return envs

    -- | Evaluate the given expression with the current globals
    eval' :: Expr -> IO ()
    eval' e  = case eval env genv e of
        Success v -> putStrLn $ S.toString $ valueToSExpression v
        Failure _ -> putStrLn "Evaluation error. Try again"
    -- eval' Nothing = putStrLn "Parse error. Try again"

    -- |Describe the given global definition
    describeGlobal :: GlobalDef -> String
    describeGlobal (Define x _) = "Variable " ++ x


