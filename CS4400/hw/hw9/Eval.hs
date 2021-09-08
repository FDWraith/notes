{-|
Module      : Eval
Description : Semantics of protoScheme
Copyright   : (c) Ferd, 2020
                  Jack, 2020
Maintainer  : f.vesely@northeastern
              gelinas.j@northeastern.edu 

This module provides the evaluator of the protoScheme language.
-}
module Eval
    ( eval
    , evalProgram
    , evalGlobal
    , runSExpression
    , runProgram
    , base
    , Eval.allTests
    ) where

import Syntax
import Maps
import qualified SExpression as S
import SimpleTestsColor (test, testSection)
import Result

import Types hiding (toSExpression, fromSExpression)

import Prelude hiding (fail)
import Control.Monad.Fail

import qualified Parser -- for runProgram tests

-- |Evaluate the given program
evalProgram :: Program -> Result Value
evalProgram (Program globals expr) = do
    genv <- evalGlobals' empty globals
    eval base genv expr
  where
    evalGlobals :: Env -> [GlobalDef] -> Result Env
    evalGlobals genv (def : globals) = do
        genv' <- evalGlobal genv def
        evalGlobals genv' globals
    evalGlobals genv [] = return genv

    -- an alternative formulation:
    evalGlobals' :: Env -> [GlobalDef] -> Result Env
    evalGlobals' genv globals = 
        foldl (\r gl -> r >>= \genv -> evalGlobal genv gl) (return genv) globals

-- |Elaborate a global definition and add it to the given environment
evalGlobal :: Env -> GlobalDef -> Result Env
evalGlobal genv (Define (TySig x _) _) | x `elem` keys genv = 
    fail $ "Global variable " ++ x ++ " is already defined"
evalGlobal genv (Define (TySig x _) expr) = do
    v <- eval base genv expr
    return $ set genv x v

-- |Evaluates the given expression to a value, relative to the given local and 
-- global environments.
eval :: Env -> Env -> Expr -> Result Value
eval _ _ (Value v) = return v
eval env genv (Var x) =
    case get env x of
         Just v -> return v
         Nothing -> fromMaybe' ("Variable " ++ x ++ " not found") $
                        get genv x
eval env genv (Lam xs e) =
    return $ Closure (map getVar xs) e env
eval env genv (Let x e1 e2) = do
    v1 <- eval env genv e1
    eval (set env x v1) genv e2
eval env genv (If e1 e2 e3) = do
    Boolean b <- eval env genv e1 
    if b then eval env genv e2
         else eval env genv e3
eval env genv (And e1 e2) = do
    Boolean b <- eval env genv e1 
    if b then eval env genv e2
         else return $ Boolean False
eval env genv (Or e1 e2) = do
    Boolean b <- eval env genv e1 
    if b then return $ Boolean True
         else eval env genv e2
eval env genv (Cond clauses els) =
    evalClauses clauses els
  where
    evalClauses [] (Just e) = eval env genv e
    evalClauses [] Nothing = fail "cond: no applicable clauses"
    evalClauses ((e1, e2) : rest) els = do
        Boolean b <- eval env genv e1 
        if b then eval env genv e2
             else evalClauses rest els
eval env genv (Call e es) = do
    v <- eval env genv e
    ensureApplicable v
    vs <- mapM (eval env genv) es
    case v of
         Closure xs body env' -> eval (extendEnv env' $ zip xs vs) genv body
         PrimApp op args -> apply op $ args ++ vs
         _ -> error "Not an applicable value." -- this shouldn't happen after ensureApplicable
  where    
    -- |fail if the argument is not a function value
    ensureApplicable :: Value -> Result ()
    ensureApplicable Closure {} = return ()
    ensureApplicable PrimApp {} = return ()
    ensureApplicable _ = 
        fail $ S.toString (toSExpression e) ++ ": not a function or primop"

    -- |apply an operation to its arguments
    apply :: Op -> [Value ] -> Result Value 
    apply op@(Op name arity f) args
        | arity < length args = fail $ name ++ ": too many arguments"
        | arity > length args = return $ PrimApp op args
        | arity == length args = f args

    -- |Extend the given environment with the given variable->value pairings
    extendEnv :: Env -> [(Variable, Value)] -> Env
    extendEnv = foldl $ \r (x, v) -> set r x v

-- |Library of built-in operations
base :: Env
base = fromList 
    [ mkPrim $ mkNumOp "+" ((+) `returns` Integer) ((+) `returns` Float)
    , mkPrim $ mkNumOp "-" ((-) `returns` Integer) ((-) `returns` Float)
    , mkPrim $ mkNumOp "*" ((*) `returns` Integer) ((*) `returns` Float)
    , mkPrim $ mkNumOp "/" intDiv fltDiv 
    , mkPrim $ mkNumOp "<" ((<) `returns` Boolean) ((<) `returns` Boolean)
    , mkPrim $ mkNumOp ">" ((>) `returns` Boolean) ((>) `returns` Boolean)
    , mkPrim $ mkNumOp "<=" ((<=) `returns` Boolean) ((<=) `returns` Boolean)
    , mkPrim $ mkNumOp ">=" ((>=) `returns` Boolean) ((>=) `returns` Boolean)
    , mkPrim $ mkBinOp "=" typedEq
    , mkPrim $ mkUnOp "not" not'
    , mkPrim $ mkBinOp "pair" $ \l r -> return $ PairV l r
    , mkPrim $ mkUnOp "left" left
    , mkPrim $ mkUnOp "right" right
    , mkPrim $ mkUnOp "real?" realHuh 
    , mkPrim $ mkUnOp "integer?" integerHuh 
    , mkPrim $ mkUnOp "number?" numberHuh 
    , mkPrim $ mkUnOp "boolean?" booleanHuh 
    , mkPrim $ mkUnOp "pair?" pairHuh
    ]
  where
    returns :: (a -> b -> c) -> (c -> Value) -> a -> b -> Result Value
    (f `returns` valueType) x y = return $ valueType $ f x y

    intDiv _ 0 = fail "/: division by zero"
    intDiv x y = (div `returns` Integer) x y

    fltDiv _ 0 = fail "/: division by zero"
    fltDiv x y = ((/) `returns` Float) x y

    not' (Boolean b) = return $ Boolean $ not b
    not' _ = fail "not: expected a boolean"

    left (PairV v _) = return v
    left _ = fail "left: expected a pair"
    
    right (PairV _ v) = return v
    right _ = fail "right: expected a pair"

    realHuh (Float _) = return $ Boolean True
    realHuh _ = return $ Boolean False

    integerHuh (Integer _) = return $ Boolean True
    integerHuh _ = return $ Boolean False

    numberHuh (Float _) = return $ Boolean True
    numberHuh (Integer _) = return $ Boolean True
    numberHuh _ = return $ Boolean False

    booleanHuh (Boolean _) = return $ Boolean True
    booleanHuh _ =  return $ Boolean False

    pairHuh (PairV _ _) = return $ Boolean True
    pairHuh _ =  return $ Boolean False

-- Construct a primop from an operation
mkPrim :: Op -> (Variable, Value)
mkPrim op@(Op name _ _) = (name, PrimApp op [])

-- |Construct a coercing numeric operation
mkNumOp 
    :: String 
    -> (Integer -> Integer -> Result Value) 
    -> (Double -> Double -> Result Value)
    -> Op
mkNumOp name intOp fltOp = Op name 2 $ coerceAndApply
  where
    coerceAndApply :: [Value] -> Result Value
    coerceAndApply [Integer i1, Integer i2] = intOp i1 i2
    coerceAndApply [v1, v2] = do 
        f1 <- toFloat v1 
        f2 <- toFloat v2
        fltOp f1 f2
    coerceAndApply _ = fail $ name ++ ": expected 2 arguments"
    
    toFloat :: Value -> Result Double
    toFloat (Integer i) = return $ fromIntegral i
    toFloat (Float f) = return f
    toFloat _ = fail $ name ++ ": incompatible arguments"

-- |Construct a unary operation
mkUnOp :: String -> (Value -> Result Value) -> Op
mkUnOp name f = 
    let op [v] = f v 
        op _ = fail $ name ++ ": expected one argument."
    in Op name 1 op

-- |Construct a binary operation
mkBinOp :: String -> (Value -> Value -> Result Value) -> Op
mkBinOp name f =
    let op [v1, v2] = f v1 v2
        op _ = fail $ name ++ ": expected two arguments"
    in Op name 2 op

-- |Check if the type of the given values matches
typeMatches :: Value -> Value -> Bool
typeMatches (Boolean _) (Boolean _) = True
typeMatches (Float _) (Float _) = True
typeMatches (Integer _) (Integer _) = True
typeMatches (PairV l1 r1) (PairV l2 r2) = typeMatches l1 l2 && typeMatches r1 r2
typeMatches _ _ = False

-- |Typed equality: returns Nothing if the types don't match
typedEq :: MonadFail m => Value -> Value -> m Value
typedEq v1 v2 | typeMatches v1 v2 = return $ Boolean $ v1 == v2
              | otherwise = fail "=: incompatible types"

-- |pulls variable names from signatures
getVar :: Signature -> Variable
getVar (TySig s ty) = s

-- |Run the given protoScheme s-expression, returning an s-expression
-- representation of the result.
runSExpression :: S.Expr -> Result S.Expr
runSExpression se = do
    e <- fromSExpression se
    v <- eval base empty e
    return $ valueToSExpression v

-- |Run the given protoScheme s-expression, returning an s-expression
-- representation of the result.
runProgram :: [S.Expr] -> Result S.Expr
runProgram es = do
    e <- programFromSExpressions es
    v <- evalProgram e
    return $ valueToSExpression v


-- Tests
allTests :: IO ()
allTests = do
    testSection "evalProgram"
    test_evalProgram
    testSection "eval"
    test_eval
    testSection "runProgram"
    test_runProgram

test_evalProgram :: IO ()
test_evalProgram = do
    test "global variable definition and lookup"
        (evalProgram $ Program [Define (TySig "x" (TyBase TyInteger)) $ integer 12] (Var "x"))
        (return $ Integer 12)
    test "use a global in a definition"
        (evalProgram $ 
            Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
                    , Define (TySig "y" (TyBase TyInteger)) $ Call (Var "+") [Var "x", Var "x"]
                    ] $
                Var "y")
        (return $ Integer 24)
    test "simple function"
        (evalProgram $ 
            Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                      $ Lam [TySig "x" (TyBase TyInteger)] (Var "x") ] $
                Call (Var "f") [integer 12])
        (return $ Integer 12)
    test "multiple arguments 1"
        (evalProgram $ 
            Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger, TyBase TyInteger]))
                      $ Lam [TySig "x" (TyBase TyInteger), TySig "y" (TyBase TyInteger), TySig "z" (TyBase TyInteger)] (Var "y") ] $
                Call (Var "f") $ map integer [1, 2, 3])
        (return $ Integer 2)
    test "multiple arguments 2"
        (evalProgram $ 
            Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger, TyBase TyInteger]))
                      $ Lam [TySig "x" (TyBase TyInteger), TySig "y" (TyBase TyInteger), TySig "z" (TyBase TyInteger)] (Var "z") ] $
                Call (Var "f") $ map integer [1, 2, 3])
        (return $ Integer 3)
    test "use a global in a function definition"
        (evalProgram $ 
            Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
                    , Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                      $ Lam [TySig "y" (TyBase TyInteger)] $ Call (Var "+") [Var "x", Var "y"]
                    ] $
                Call (Var "f") [ Call (Var "-") [ integer 0, Var "x" ] ])
        (return $ Integer 0)
    test "recursive function (factorial)"
        (evalProgram $ 
            Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                      $ Lam [TySig "x" (TyBase TyInteger)] $
                          If (Call (Var "<") [ Var "x", integer 2])
                             (integer 1)
                             (Call (Var "*") 
                                 [ Var "x"
                                 , Call (Var "f") 
                                       [ Call (Var "-") 
                                             [ Var "x"
                                             , integer 1 
                                             ] 
                                       ] 
                                 ])
                    ] $
                Call (Var "f") [ integer 10 ])
        (return $ Integer $ product [2..10])
    test "local shodowing 1"
        (evalProgram $
            Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12 ] $
                Let "x" (integer 42) (Var "x"))
        (return $ Integer 42)
    test "local shodowing 2"
        (evalProgram $
            Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12 
                    , Define (TySig "y" (TyBase TyInteger)) $ Call (Var "+") [Var "x", integer 1]
                    ] $
                Let "x" (Call (Var "-") [Var "x", integer 1]) 
                    (Call (Var "pair") [Var "x", Var "y"]))
        (return $ PairV (Integer 11) (Integer 13))
    test "global overriding fails"
        (evalProgram $ 
            Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
                    , Define (TySig "x" (TyBase TyInteger)) $ integer 13
                    ]
              (Var "x"))
        (Failure "Global variable x is already defined")


test_eval :: IO ()
test_eval = do
    test "eval (+ 21 23)" 
        (eval' $ Call (Var "+") [integer 21, integer 23])
        (return (Integer 44))
    test "eval (- 21.3 23)" 
        (eval' $ Call (Var "-") [float 21.3, integer 23])
        (return $ Float (21.3 - 23))
    test "eval (* 3 1.2)" 
        (eval' $ mul (integer 3) (float 1.2))
        (return $ Float (3 * 1.2))
    test "eval (/ 5 2)"
        (eval' $ div' (integer 5) (integer 2))
        (return $ Integer 2)
    test "eval (/ 5.0 2)"
        (eval' $ div' (float 5) (integer 2))
        (return $ Float 2.5)
    test "eval (/ 5 2.0)"
        (eval' $ div' (integer 5) (float 2))
        (return $ Float 2.5)
    test "eval (/ 5 0)"
        (eval' $ div' (integer 5) (integer 0))
        (Failure "/: division by zero")
    test "eval (let (x 1) x)"
        (eval' $ Let "x" (integer 1) (Var "x"))
        (return $ Integer 1)
    test "eval (let (x 1) y)"
        (eval' $ Let "x" (integer 1) (Var "y"))
        (Failure "Variable y not found")
    test "eval (let (x 12) (+ x (let (x (+ x 1)) (+ x (let (x (+ x 1)) x)))))"
        (eval' $ Let "x" (integer 12) 
                    (add (Var "x")
                         (Let "x" (add (Var "x") (integer 1))
                              (add (Var "x")
                                   (Let "x" (add (Var "x") (integer 1))
                                        (Var "x"))))))
        (return $ Integer (let x = 12 in x + let y = x + 1 in y + let z = y + 1 in z))
    test "eval (if (and #t (not #f)) (or (not #t) #f) 42)"
        (eval' $ If (And (bool True) (not' (bool False)))
                   (Or (not' (bool True)) (bool False))
                   (integer 42))
        (return $ Boolean False)
    test "eval (and #f (/ 1 0))"
        (eval' $ And (bool False) (div' (integer 1) (integer 0)))
        (return $ Boolean False)
    test "eval (and (/ 1 0) #f)"
        (eval' $ And (div' (integer 1) (integer 0)) (bool False))
        (Failure "/: division by zero") 
    test "eval (or #t (/ 1 0))"
        (eval' $ Or (bool True) (div' (integer 1) (integer 0)))
        (return $ Boolean True)
    test "eval (and (> 1 0) (< 0 1))"
        (eval' $ And (gt (integer 1) (integer 0)) (lt (integer 0) (integer 1)))
        (return $ Boolean True)
    test "eval (and (> 1.1 0) (< -1 0.1))"
        (eval' $ And (gt (float 1.1) (integer 0)) (lt (integer $ -1) (float 0.1)))
        (return $ Boolean True)
    test "eval (< #t 1)"
        (eval' $ lt (bool True) (integer 1))
        (Failure "<: incompatible arguments") 
    test "eval (= #t #t)"
        (eval' $ eq (bool True) (bool True))
        (return $ Boolean True)
    test "eval (= #t #f)"
        (eval' $ eq (bool True) (bool False))
        (return $ Boolean False)
    test "eval (= 1 1)"
        (eval' $ eq (integer 1) (integer 1))
        (return $ Boolean True)
    test "eval (= #f 0)"
        (eval' $ eq (bool False) (integer 0))
        (Failure "=: incompatible types")
    test "eval (= 1.0 1)"
        (eval' $ eq (float 1) (integer 1))
        (Failure "=: incompatible types")
    test "eval (cond)"
        (eval' $ Cond [] Nothing)
        (Failure "cond: no applicable clauses")
    test "eval (cond (else 42))"
        (eval' $ Cond [] (return $ integer 42))
        (return $ Integer 42)
    test "eval (cond ((= 2 2) 1) (else 42))"
        (eval' $ Cond [(eq (integer 2) (integer 2), integer 1)] (return $ integer 42))
        (return $ Integer 1)
    test "eval (cond ((= 2 2) 1) ((= 3 3) 2) (else 42))"
        (eval' $ Cond [ (eq (integer 2) (integer 2), integer 1)
                     , (eq (integer 3) (integer 3), integer 2)
                     ] (return $ integer 42))
        (return $ Integer 1)
    test "eval (cond ((= 2 3) 1) ((= 3 3) 2))"
        (eval' $ Cond [ (eq (integer 2) (integer 3), integer 1)
                     , (eq (integer 3) (integer 3), integer 2)
                     ] Nothing)
        (return $ Integer 2)
    test "eval (cond ((= 2 3) 1) ((= 3 4) 2) (else 42))"
        (eval' $ Cond [ (eq (integer 2) (integer 3), integer 1)
                     , (eq (integer 3) (integer 4), integer 2)
                     ] (return $ integer 42))
        (return $ Integer 42)
    test "eval (cond ((= 2 3) 1) ((= 3 4) 2))"
        (eval' $ Cond [ (eq (integer 2) (integer 3), integer 1)
                     , (eq (integer 3) (integer 4), integer 2)
                     ] Nothing)
        (Failure "cond: no applicable clauses")
    test "eval (pair (pair 1 2) (pair 3 (+ 2 2)))"
        (eval' $ 
            pair (pair (integer 1) (integer 2)) 
                 (pair (integer 3) (add (integer 2) (integer 2))))
        (return $ PairV (PairV (Integer 1) (Integer 2))
                      (PairV (Integer 3) (Integer 4)))
    test "eval (left (right (pair (pair 1 2) (pair 3 4))))"
        (eval' $ 
            pleft (pright (pair (pair (integer 1) (integer 2)) 
                          (pair (integer 3) (integer 4)))))
        (return $ Integer 3)
    test "eval (right (left (pair (pair 1 2) (pair 3 4))))"
        (eval' $ 
            pright (pleft (pair (pair (integer 1) (integer 2)) 
                          (pair (integer 3) (integer 4)))))
        (return $ Integer 2)
    test "eval (left (+ 1 2))"
        (eval' $ pleft (add (integer 1) (integer 2)))
        (Failure "left: expected a pair")
    test "eval (right (and #t #f))"
        (eval' $ pright (And (bool True) (bool False)))
        (Failure "right: expected a pair")
  where
    eval' = eval base empty
    binApp f e1 e2 = Call (Var f) [e1, e2]
    add = binApp "+"
    sub = binApp "-"
    mul = binApp "*"
    div' = binApp "/"
    lt = binApp "<"
    gt = binApp ">"
    eq = binApp "="
    not' e = Call (Var "not") [e]
    pair = binApp "pair"
    pleft e = Call (Var "left") [e]
    pright e = Call (Var "right") [e]

test_runProgram :: IO ()
test_runProgram = do
    test "no definitions"
        (runProgram 
            [ S.List 
                [ S.Symbol "let"
                , S.List [ S.Symbol "x", S.Integer 11 ]
                , S.List [ S.Symbol "*", S.Symbol "x", S.Symbol "x" ]
                ]
            ])
        (valueToSExpression <$> 
            eval base empty 
                (Let "x" (integer 11) (Call (Var "*") [ Var "x", Var "x" ])))
    test ("runProgram \"" ++ p1 ++ "\"")
        (Parser.parseSExpressions p1 >>= runProgram)
        (return $ S.Integer 15)
  where 
    p1 = "(x : Integer) (define x 12) (f : (-> Integer Integer Integer)) (defun f (x y) (if (= x 0) y (f (- x 1) (+ y 1)))) (f x 3)"

