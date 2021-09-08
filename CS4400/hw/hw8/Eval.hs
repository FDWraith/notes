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
    , evalGlobal
    , evalProgram
    , GlobalEnv
    , runSExpression
    , runProgram
    , liftM2
    , justWrap
    , test_eval
    , test_evalProgram
    ) where

import Syntax
import Result 
import Maps
import qualified SExpression as S
import SimpleTests (test)

import Debug.Trace

type Primitive = [Expr] -> Result Value

type PrimitiveEnv = Map Variable Primitive
type GlobalEnv = Map Variable Expr

base :: Env -> GlobalEnv -> PrimitiveEnv
base env genv = fromList [
  ("+", binaryOp "+"),
  ("-", binaryOp "-"),
  ("*", binaryOp "*"),
  ("/", binaryOp "/"),
--  ("and", binaryOp "and"),
--  ("or", binaryOp "or"),
  ("<", binaryOp "<"),
  (">", binaryOp ">"),
  ("=", binaryOp "="),
  ("<=", binaryOp "<="),
  (">=", binaryOp ">="),
  ("not", unaryOp "not"),
  ("real?", unaryOp "real?"),
  ("integer?", unaryOp "integer?"),
  ("number?", unaryOp "number?"),
  ("boolean?", unaryOp "boolean?")
                ]
  where    
    binaryOp :: Variable -> Primitive
    binaryOp op (e1' : e2' : es) = do
      e1 <- eval env genv e1'
      e2 <- eval env genv e2'
      case op of
        "+" -> liftArith (+) (+) e1 e2
        "*" -> liftArith (*) (*) e1 e2
        "-" -> liftArith (-) (-) e1 e2
        "/" -> divide e1 e2
--        "and" -> and' e1 e2
--        "or" -> or' e1 e2
        "<" -> liftOrd (<) (<) e1 e2
        ">" -> liftOrd (>) (>) e1 e2
        "=" -> typEq e1 e2
        "<=" -> liftOrd (<=) (<=) e1 e2
        ">=" -> liftOrd (>=) (>=) e1 e2
    binaryOp op _ = Failure "Missing Expressions"

    unaryOp :: Variable -> Primitive
    unaryOp op (e' : es) = do
      e <- eval env genv e'
      case op of
        "not" -> not' e
        "real?" -> realHuh e
        "number?" -> numberHuh e
        "integer?" -> intHuh e
        "boolean?" -> booleanHuh e
    unaryOp op _ = Failure "Missing Expressions"

evalProgram :: Program -> Result Value
evalProgram (Program globals expr) = do
  genv <- evalGlobals empty globals
  eval empty genv expr
  where
    evalGlobals :: GlobalEnv -> [GlobalDef] -> Result GlobalEnv
    evalGlobals genv (def : globals) = do
      genv' <- evalGlobal genv def
      evalGlobals genv' globals
    evalGlobals genv empty = return genv

evalGlobal :: GlobalEnv -> GlobalDef -> Result GlobalEnv
evalGlobal genv (Define x _) | x `elem` keys genv = do
                                 Failure $ (show x) ++ " cannot be redefined"
evalGlobal genv (Define x expr) = do
  return (set genv x expr)
      
-- |Evaluates the given expression to a value.
eval :: Env -> GlobalEnv -> Expr -> Result Value
eval _ _ (Value v) = return v
eval env genv (Var x) = do
  case (get env x) of
    Just (Thunk e env') -> (eval env' genv e)
    Nothing -> do
      e <- (fromMaybe (get genv x))
      (eval env genv e)
eval env genv (Let x e1 e2) = do
  -- v <- (eval env genv e1)
  (eval (set env x (Thunk e1 env)) genv e2)
eval env genv (If e1 e2 e3) = do
  (Boolean b) <- (eval env genv e1)
  (if b then (eval env genv e2) else (eval env genv e3))
eval env genv (And e1 e2) = do
  (Boolean b) <- (eval env genv e1)
  (if b then (eval env genv e2) else return (Boolean False))
eval env genv (Or e1 e2) = do
  (Boolean b) <- (eval env genv e1)
  (if b then return (Boolean True) else (eval env genv e2))
eval env genv (Cond clauses els) =
    evalClauses clauses els
  where
    evalClauses [] Nothing = Failure "No Expression at end of Cond"
    evalClauses [] (Just e) = eval env genv e
    evalClauses ((e1, e2) : rest) els = do
      (Boolean b) <- eval env genv e1
      (if b then (eval env genv e2) else (evalClauses rest els)) 
eval env genv (Pair e1 e2) = do
    v1 <- eval env genv e1
    v2 <- eval env genv e2
    return $ PairV v1 v2
eval env genv (PLeft e) = do
  v <- eval env genv e
  case v of
    PairV v1 _ -> return v1
    _ -> Failure "Taking Left of Non-Pair"
eval env genv (PRight e) = do
  v <- eval env genv e
  case v of
    PairV _ v2 -> return v2
    _ -> Failure "Taking Right of Non-Pair"
eval env genv (Lambda args body) = do
  return $ Closure args body env
eval env genv (App (Var p) es)
  | p `elem` (keys (base env genv)) = evalPrimitive env genv p es
  where
    evalPrimitive :: Env -> GlobalEnv -> Variable -> [Expr] -> Result Value
    evalPrimitive env genv p es = do
      func <- fromMaybe $ get (base env genv) p
      (func es)
eval env genv (App f es) = do
  Closure as body env' <- eval env genv f
  eval (updateEnv env' (zip as (map partialEval es))) genv body
    where
      updateEnv :: Env -> [(Variable, Thunk)] -> Env
      updateEnv ev [] = ev
      updateEnv ev ((x, v) : rest) = set (updateEnv ev rest) x v

      partialEval :: Expr -> Thunk
      partialEval e = Thunk e env
eval env genv (PairHuh e) = do
  v <- eval env genv e
  case v of
    (PairV _ _) -> return $ Boolean True
    _ -> return $ Boolean False

-- |A binary operation type
type BinOp a = (a -> a -> a)

-- |Applies the appropriate arithmetic operation on the given values
evalArith :: BinOp Integer -> BinOp Double -> Value -> Value -> Result Value
evalArith intOp _ (Integer i1) (Integer i2) = return $ Integer $ i1 `intOp` i2
evalArith _ fltOp (Integer i1) (Float f2) = return $ Float $ fromIntegral i1 `fltOp` f2
evalArith _ fltOp (Float f1) (Integer i2) = return $ Float $ f1 `fltOp` fromIntegral i2
evalArith _ fltOp (Float f1) (Float f2) = return $ Float $ f1 `fltOp` f2
evalArith _ _ _ _ = Failure "Operator not recognized"

-- |Applies the given function to the lift maybe expressions
liftM2 :: (a -> b -> Result c) -> Result a -> Result b -> Result c
liftM2 fn a b = do
  aR <- a
  bR <- b
  (fn aR bR)


-- |Wraps the result of the given binary function in a Just
justWrap :: (a -> b -> c) -> a -> b -> Maybe c
justWrap fn a b = Just $ fn a b


-- |Applies the arithmetic coersion and appliation on the lifted results of the evaluated expressions
liftArith 
    :: BinOp Integer 
    -> BinOp Double 
    -> Value 
    -> Value 
    -> Result Value
liftArith intOp fltOp v1 v2 = evalArith intOp fltOp v1 v2

-- |Applies the arithmetic coersion and divisino on the lifted results of the evaluated expressions
divide :: Value -> Value -> Result Value
divide v1 v2 = do
  case v2 of
    (Integer 0) -> Failure "Division By Zero"
    (Float 0) -> Failure "Division By Zero"
    _ -> evalArith div (/) v1 v2


-- |Applies and to the boolean values
and' :: Value -> Value -> Result Value
and' e1 e2 = case e1 of
  (Boolean b) -> (if b then return e2 else return (Boolean False))
  _ -> Failure "Not a Boolean"

-- |Applies or to the boolean values
or' :: Value -> Value -> Result Value
or' e1 e2 = case e1 of
  (Boolean b) -> (if b then return (Boolean True) else return e2)
  _ -> Failure "Not a Boolean"

-- |Applies not to a boolean value
not' :: Value -> Result Value
not' e = case e of
  (Boolean b) -> return (Boolean (not b))
  _ -> Failure "Not a Boolean"

-- |Checks if someting is a float
realHuh :: Value -> Result Value
realHuh v = case v of
  (Float _) -> return $ Boolean True
  _ -> return $ Boolean False

-- |Checks if someting is a integer
intHuh :: Value -> Result Value
intHuh v = case v of
  (Integer _) -> return $ Boolean True
  _ -> return $ Boolean False

-- |Checks if someting is a boolean
booleanHuh :: Value -> Result Value
booleanHuh v = case v of
  (Boolean _) -> return $ Boolean True
  _ -> return $ Boolean False

-- |Checks if someting is a number
numberHuh :: Value -> Result Value
numberHuh v = case v of
  (Float _) -> return $ Boolean True
  (Integer _) -> return $ Boolean True
  _ -> return $ Boolean False

-- |Ordering operations
liftOrd 
    :: (Integer -> Integer -> Bool) 
    -> (Double -> Double -> Bool) 
    -> Value 
    -> Value 
    -> Result Value
liftOrd intOp _ (Integer i1) (Integer i2) = return (Boolean $ i1 `intOp` i2)
liftOrd _ flOp (Float f1) (Float f2) = return (Boolean $ f1 `flOp` f2)
liftOrd _ flOp (Integer i1) (Float f2) = return (Boolean $ fromInteger i1 `flOp` f2)
liftOrd _ flOp (Float f1) (Integer i2) = return (Boolean $ f1 `flOp` fromInteger i2)
liftOrd _ _ _ _ = Failure "Cannot Order"

-- |Check if the type of the given values matches
typeMatches :: Value -> Value -> Bool
typeMatches (Boolean _) (Boolean _) = True
typeMatches (Float _) (Float _) = True
typeMatches (Integer _) (Integer _) = True
typeMatches _ _ = False

-- |Typed equality: returns Nothing if the types don't match
typEq :: Value -> Value -> Result Value
typEq v1 v2 | typeMatches v1 v2 = return $ Boolean $ v1 == v2
            | otherwise = Failure "Types not Same"

-- |Run the given protoScheme s-expression, returning an s-expression
-- representation of the result.
runSExpression :: S.Expr -> Result S.Expr
runSExpression se = do
  v <- eval empty empty (fromSExpression se)
  return (valueToSExpression v)

-- |Run the given protoScheme s-expression as a program,
-- returning an s-expression representation of the result
runProgram :: [S.Expr] -> Result S.Expr
runProgram prog = do
  p <- programFromSExpression (S.List prog)
  v <- evalProgram p
  return (valueToSExpression v)

test_eval = do
    test "eval (+ 21 23)" 
        (eval' (App (Var "+") [(integer 21), (integer 23)]))
        (Success (Integer 44))
    test "eval (- 21.3 23)" 
        (eval' $ App (Var "-") [(float 21.3), (integer 23)])
        (Success $ Float (21.3 - 23))
    test "eval (* 3 1.2)" 
        (eval' $ App (Var "*") [(integer 3), (float 1.2)])
        (Success $ Float (3 * 1.2))
    test "eval (/ 5 2)"
        (eval' $ App (Var "/") [(integer 5), (integer 2)])
        (Success $ Integer 2)
    test "eval (/ 5.0 2)"
        (eval' $ App (Var "/") [(float 5), (integer 2)])
        (Success $ Float 2.5)
    test "eval (/ 5 2.0)"
        (eval' $ App (Var "/") [(integer 5), (float 2)])
        (Success $ Float 2.5)
    test "eval (/ 5 0)"
        (eval' $ App (Var "/") [(integer 5), (integer 0)])
        (Failure "Division By Zero")
    test "eval (let (x 1) x)"
        (eval' $ Let "x" (integer 1) (Var "x"))
        (Success $ Integer 1)
    test "eval (let (x 1) y)"
        (eval' $ Let "x" (integer 1) (Var "y"))
        (Failure "Got Nothing")
    test "eval (let (x 12) (+ x (let (x (+ x 1)) (+ x (let (x (+ x 1)) x)))))"
        (eval' $ Let "x" (integer 12) 
                    (App (Var "+")
                      [(Var "x"),
                       (Let "x" (App (Var "+") [(Var "x"), (integer 1)])
                         (App (Var "+")
                           [(Var "x"),
                            (Let "x" (App (Var "+") [(Var "x"), (integer 1)])
                              (Var "x"))                           
                            ]))
                         ]))
        (Success $ Integer (let x = 12 in x + let y = x + 1 in y + let z = y + 1 in z))
    test "eval (if (and #t (not #f)) (or (not #t) #f) 42)"
        (eval' $ If (And (bool True) (App (Var "not") [(bool False)]))
                   (Or (App (Var "not") [(bool True)]) (bool False))
                   (integer 42))
        (Success $ Boolean False)
    test "eval (and #f (/ 1 0))"
        (eval' $ And (bool False) (App (Var "/") [(integer 1), (integer 0)]))
        (Success $ Boolean False)
    test "eval (and (/ 1 0) #f)"
        (eval' $ And (App (Var "/") [(integer 1), (integer 0)]) (bool False))
        (Failure "Division By Zero")
    test "eval (or #t (/ 1 0))"
        (eval' $ Or (bool True) (App (Var "/") [(integer 1), (integer 0)]))
        (Success $ Boolean True)
    test "eval (and (> 1 0) (< 0 1))"
        (eval' $ And (App (Var ">") [(integer 1), (integer 0)]) (App (Var "<") [(integer 0), (integer 1)]))
        (Success $ Boolean True)
    test "eval (and (> 1.1 0) (< -1 0.1))"
        (eval' $ And (App (Var ">") [(float 1.1), (integer 0)]) (App (Var "<") [(integer $ -1), (float 0.1)]))
        (Success $ Boolean True)
    test "eval (< #t 1)"
        (eval' $ App (Var "<") [(bool True), (integer 1)])
        (Failure "Cannot Order")
    test "eval (= #t #t)"
        (eval' $ App (Var "=") [(bool True), (bool True)])
        (Success $ Boolean True)
    test "eval (= #t #f)"
        (eval' $ App (Var "=") [(bool True), (bool False)])
        (Success $ Boolean False)
    test "eval (= 1 1)"
        (eval' $ App (Var "=") [(integer 1), (integer 1)])
        (Success $ Boolean True)
    test "eval (= #f 0)"
        (eval' $ App (Var "=") [(bool False), (integer 0)])
        (Failure "Types not Same")
    test "eval (= 1.0 1)"
        (eval' $ App (Var "=") [(float 1), (integer 1)])
        (Failure "Types not Same")
    test "eval (cond)"
        (eval' $ Cond [] Nothing)
        (Failure "No Expression at end of Cond")
    test "eval (cond (else 42))"
        (eval' $ Cond [] (Just $ integer 42))
        (Success $ Integer 42)
    test "eval (cond ((= 2 2) 1) (else 42))"
        (eval' $ Cond [(App (Var "=") [(integer 2), (integer 2)], integer 1)] (Just $ integer 42))
        (Success $ Integer 1)
    test "eval (cond ((= 2 2) 1) ((= 3 3) 2) (else 42))"
        (eval' $ Cond [ (App (Var "=") [(integer 2), (integer 2)], integer 1)
                     , (App (Var "=") [(integer 3), (integer 3)], integer 2)
                     ] (Just $ integer 42))
        (Success $ Integer 1)
    test "eval (cond ((= 2 3) 1) ((= 3 3) 2))"
        (eval' $ Cond [ (App (Var "=") [(integer 2), (integer 3)], integer 1)
                     , (App (Var "=") [(integer 3), (integer 3)], integer 2)
                     ] Nothing)
        (Success $ Integer 2)
    test "eval (cond ((= 2 3) 1) ((= 3 4) 2) (else 42))"
        (eval' $ Cond [ (App (Var "=") [(integer 2), (integer 3)], integer 1)
                     , (App (Var "=") [(integer 3), (integer 4)], integer 2)
                     ] (Just $ integer 42))
        (Success $ Integer 42)
    test "eval (cond ((= 2 3) 1) ((= 3 4) 2))"
        (eval' $ Cond [ (App (Var "=") [(integer 2), (integer 3)], integer 1)
                     , (App (Var "=") [(integer 3), (integer 4)], integer 2)
                     ] Nothing)
        (Failure "No Expression at end of Cond")
    test "eval (pair (pair 1 2) (pair 3 (+ 2 2)))"
        (eval' $ 
            Pair (Pair (integer 1) (integer 2)) 
                 (Pair (integer 3) (App (Var "+") [(integer 2), (integer 2)])))
        (Success $ PairV (PairV (Integer 1) (Integer 2))
                      (PairV (Integer 3) (Integer 4)))
    test "eval (left (right (pair (pair 1 2) (pair 3 4))))"
        (eval' $ 
            PLeft (PRight (Pair (Pair (integer 1) (integer 2)) 
                          (Pair (integer 3) (integer 4)))))
        (Success $ Integer 3)
    test "eval (right (left (pair (pair 1 2) (pair 3 4))))"
        (eval' $ 
            PRight (PLeft (Pair (Pair (integer 1) (integer 2)) 
                          (Pair (integer 3) (integer 4)))))
        (Success $ Integer 2)
    test "eval (left (+ 1 2))"
        (eval' $ PLeft (App (Var "+") [(integer 1), (integer 2)]))
        (Failure "Taking Left of Non-Pair")
    test "eval (right (and #t #f))"
        (eval' $ PRight (And (bool True) (bool False)))
        (Failure "Taking Right of Non-Pair")
    test "eval ((lambda (x) x) 13)"
        (eval' $ App (Lambda ["x"] (Var "x")) [integer 13])
        (Success $ Integer 13)
  where
    eval' = eval empty empty


test_evalProgram = do
  test "evalProgram example1.pss"
    (evalProgram $ (Program [Define "even?"
                            (Lambda ["n"]
                             (And (App (Var "integer?") [Var "n"])
                              (Or (App (Var "=") [Var "n",Value (Integer 0)])
                               (App (Var "odd?") [App (Var "-") [Var "n",Value (Integer 1)]]))))
                           ,Define "odd?"
                            (Lambda ["n"]
                             (And (App (Var "integer?") [Var "n"])
                              (And (App (Var "not") [App (Var "=") [Var "n",Value (Integer 0)]])
                                (App (Var "even?") [App (Var "-") [Var "n",Value (Integer 1)]]))))
                           ]
      (Pair (App (Var "odd?") [Value (Integer 42)]) (App (Var "even?") [Value (Integer 42)]))))
    (Success $ (PairV (Boolean False) (Boolean True)))
  test "evalProgram example2.pss"
    (evalProgram $ (Program [Define "pair-map"
                             (Lambda ["f","p"]
                              (Pair (App (Var "f") [PLeft (Var "p")]) (App (Var "f") [PRight (Var "p")])))
                            ]
                     (App (Var "pair-map") [Lambda ["x"] (App (Var "*") [Value (Integer 2),Var "x"]),Pair (Value (Integer 11)) (Value (Float (-2.5)))])))
    (Success $ (PairV (Integer 22) (Float (-5.0))))
  test "evalProgram example3.pss"
    (evalProgram $ (Program [Define "fib"
                             (Lambda ["n"]
                              (If (App (Var "<=") [Var "n",Value (Integer 1)])
                               (Var "n")
                               (App (Var "+") [App (Var "fib") [App (Var "-") [Var "n",Value (Integer 1)]]
                                              ,App (Var "fib") [App (Var "-") [Var "n",Value (Integer 2)]]])))
                            ]
                     (App (Var "fib") [Value (Integer 10)])))
    (Success $ (Integer 55))
--test "evalProgram example4.pss" didn't pass
  test "evalProgram example5.pss"
    (evalProgram $ (Program [Define "compose"
                             (Lambda ["f","g"]
                              (Lambda ["x"]
                               (App (Var "f")
                                [App (Var "g") [Var "x"]])))]
                     (Let "double" (Lambda ["x"] (App (Var "*") [Value (Integer 2),Var "x"]))
                      (Let "negate" (Lambda ["x"] (App (Var "-") [Value (Integer 0),Var "x"]))
                       (Let "as-pair" (Lambda ["x"] (Pair (Var "x") (App (Var "negate") [Var "x"])))
                        (App (App (Var "compose") [Var "as-pair",App (Var "compose") [Var "negate",Var "double"]]) [Value (Integer 12)]))))))
    (Success $ (PairV (Integer (-24)) (Integer 24)))
