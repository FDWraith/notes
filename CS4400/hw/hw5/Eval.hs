{-|
Module      : Eval
Description : Semantics of protoScheme
Copyright   : (c) Ferd, 2020
                  Kevin Zhang, 2020
Maintainer  : f.vesely@northeastern
              zhang.kevi@northeastern.edu

This module provides the evaluator of the protoScheme language.

-}
module Eval where

import Syntax

import Maps

import qualified SExpression as S

import SimpleTests (test)

-- |Evaluates the given expression with the given context to a  value.
eval :: GlobalEnv -> Expr -> Maybe Value
eval m (Integer i) = Just (IntegerValue i)
eval m (Real r) = Just (DoubleValue r)
eval m (Boolean b) = Just (BooleanValue b)
eval m (Var x) =
  case get m x of
    Just (Define v e) -> eval m e
    _ -> Nothing
eval m (Add e1 e2) = 
    case eval m e1 of
         Just (IntegerValue v1) -> 
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 + v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) + v2))
                 Just (BooleanValue v2) -> error "Addition with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 + (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 + v2))
                 Just (BooleanValue v2) -> error "Addition with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Addition with Booleans"
         Nothing -> Nothing
eval m (Sub e1 e2) =
    case eval m e1 of
         Just (IntegerValue v1) -> 
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 - v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) - v2))
                 Just (BooleanValue v2) -> error "Subtraction with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 - (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 - v2))
                 Just (BooleanValue v2) -> error "Subtraction with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Subtraction with Booleans"
         Nothing -> Nothing
eval m (Mul e1 e2) =
    case eval m e1 of
         Just (IntegerValue v1) -> 
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 * v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) * v2))
                 Just (BooleanValue v2) -> error "Multiplication with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 * (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 * v2))
                 Just (BooleanValue v2) -> error "Multiplication with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Multiplication with Booleans"
         Nothing -> Nothing
eval m (Div e1 e2) =
    case eval m e1 of
         Just (IntegerValue v1) -> 
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 `div` v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) / v2))
                 Just (BooleanValue v2) -> error "Division with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval m e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 / (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 / v2))
                 Just (BooleanValue v2) -> error "Division with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Division with Booleans"
         Nothing -> Nothing
eval m (Let x e1 e2) = 
    case eval m e1 of
         Just v1 -> eval m (subst x v1 e2)
         Nothing -> Nothing
eval m (If cond e1 e2) =
    case eval m cond of
         Just (BooleanValue True) -> eval m e1
         Just (BooleanValue False) -> eval m e2
         _ -> Nothing         
eval m (And e1 e2) =
    case eval m e1 of
         Just (BooleanValue True) ->
            case eval m e2 of
              Just (BooleanValue v) -> Just $ BooleanValue v
              _ -> Nothing
         Just (BooleanValue False) -> Just $ BooleanValue False
         _ -> Nothing
eval m (Or e1 e2) =
    case eval m e1 of
         Just (BooleanValue True) -> Just $ BooleanValue True
         Just (BooleanValue False) ->
            case eval m e2 of
               Just (BooleanValue v) -> Just $ BooleanValue v
               _ -> Nothing
         _ -> Nothing
eval m (Not e) =
  case eval m e of
    Just (BooleanValue True) -> Just $ BooleanValue False
    Just (BooleanValue False) -> Just $ BooleanValue True
    _ -> Nothing
eval m (GreaterThan e1 e2) =
  case eval m e1 of
    Just (IntegerValue v1) ->
      case eval m e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 > v2)
        Just (DoubleValue v2) -> Just $ BooleanValue ((fromInteger v1) > v2)
        _ -> Nothing
    Just (DoubleValue v1) ->
      case eval m e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 > (fromInteger v2))
        Just (DoubleValue v2) -> Just $ BooleanValue (v1 > v2)
    _ -> Nothing
eval m (LessThan e1 e2) =
  case eval m e1 of
    Just (IntegerValue v1) ->
      case eval m e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 < v2)
        Just (DoubleValue v2) -> Just $ BooleanValue ((fromInteger v1) < v2)
        _ -> Nothing
    Just (DoubleValue v1) ->
      case eval m e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 < (fromInteger v2))
        Just (DoubleValue v2) -> Just $ BooleanValue (v1 < v2)
    _ -> Nothing
eval m (Equals e1 e2) =
  case eval m e1 of
    Just (IntegerValue v1) ->
      case eval m e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 == v2)
        Just (DoubleValue v2) -> Just $ BooleanValue ((fromInteger v1) == v2)
        _ -> Nothing
    Just (DoubleValue v1) ->
      case eval m e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 == (fromInteger v2))
        Just (DoubleValue v2) -> Just $ BooleanValue (v1 == v2)
    Just (BooleanValue v1) ->
      case eval m e2 of
        Just (BooleanValue v2) -> Just $ BooleanValue (v1 == v2)
        _ -> Nothing
    Nothing -> Nothing
eval m (Cond conds) =
  case conds of
    (WithoutElse []) -> Nothing
    (WithoutElse ((ClauseIf e1 e2):xs)) ->
      case eval m e1 of
        Just (BooleanValue True) -> eval m e2
        Just (BooleanValue False) -> eval m (Cond (WithoutElse xs))
        _ -> Nothing
    (WithElse [] (Else e)) -> eval m e
    (WithElse ((ClauseIf e1 e2):xs) el) ->
      case eval m e1 of
        Just (BooleanValue True) -> eval m e2
        Just (BooleanValue False) -> eval m (Cond (WithElse xs el))
        _ -> Nothing
eval m (Pair e1 e2) =
  case eval m e1 of
    Just v1 ->
      case eval m e2 of
        Just v2 -> Just $ PairValue v1 v2
        _ -> Nothing
    _ -> Nothing
eval m (PLeft e) =
  case e of
    (Pair v1 _) -> (eval m v1)
    _ -> Nothing
eval m (PRight e) =
  case e of
    (Pair _ v2) -> (eval m v2)
    _ -> Nothing
eval m (IsInteger e) =
  case eval m e of
    Just (IntegerValue v) -> Just $ BooleanValue True
    Just _ -> Just $ BooleanValue False
    Nothing -> Nothing
eval m (IsReal e) =
  case eval m e of
    Just (DoubleValue v) -> Just $ BooleanValue True
    Just _ -> Just $ BooleanValue False
    Nothing -> Nothing
eval m (IsNumber e) =
  case eval m e of
    Just (IntegerValue v) -> Just $ BooleanValue True
    Just (DoubleValue v) -> Just $ BooleanValue True
    Just _ -> Just $ BooleanValue False
    Nothing -> Nothing
eval m (IsPair e) =
  case eval m e of
    Just (PairValue v v2) -> Just $ BooleanValue True
    Just _ -> Just $ BooleanValue False
    Nothing -> Nothing
eval m (IsBoolean e) =
  case eval m e of
    Just (BooleanValue v) -> Just $ BooleanValue True
    Just _ -> Just $ BooleanValue False
    Nothing -> Nothing
eval m (Call f e es) =
  case get m f of
    Just (Defun _ as expr) -> eval m (substValues (as) (asValues (e:es)) expr)
    _ -> Nothing
  where asValues [] = []
        asValues (e:es) =
          case eval m e of
            Just v -> [v] ++ (asValues es)
            _ -> error "failed to call function"

test_eval = do
    test "eval: (+ 12 30)" (eval empty (Add (Integer 12) (Integer 30))) (Just (IntegerValue 42))
    test "eval: (let (x (+1 2)) (* 4 x))" 
       (eval empty $ Let "x" (Add (Integer 1) (Integer 2)) (Mul (Integer 4) (Var "x")))
       (Just $ IntegerValue 12)
    test "eval: (+ 32 14)" (eval empty ex_expr_3) (Just $ IntegerValue 46)
    test "eval: (/ 15 5)" (eval empty ex_expr_4) (Just $ IntegerValue 3)
    test "eval: (* (+ 25 13) (- 12 11))" (eval empty ex_expr_5) (Just $ IntegerValue 38)
    test "eval: (let (\"x\" 21) (+ \"x\" 23))" (eval empty ex_expr_6) (Just $ IntegerValue 44)
    test "eval: (/ 3.14 10)" (eval empty (Div (Real 3.14) (Integer 10))) (Just $ DoubleValue 0.314)
    test "eval: (if #t 42 0)" (eval empty ex_expr_8) (Just $ IntegerValue 42)
    test "eval: (and #f (/ 42 0))" (eval empty ex_expr_9) (Just $ BooleanValue False)
    test "eval: (and #t #t)" (eval empty (And (Boolean True) (Boolean True))) (Just $ BooleanValue True)
    test "eval: (or #t (/ 42 0))" (eval empty ex_expr_10) (Just $ BooleanValue True)
    test "eval: (or #f (not #t))" (eval empty (Or (Boolean False) ex_expr_11)) (Just $ BooleanValue False)
    test "eval: (> 42 10)" (eval empty ex_expr_12) (Just $ BooleanValue True)
    test "eval: (< 5 1)" (eval empty ex_expr_13) (Just $ BooleanValue False)
    test "eval: (= 10 10)" (eval empty ex_expr_14) (Just $ BooleanValue True)
    test "eval: (= (not #t) #f)" (eval empty ex_expr_15) (Just $ BooleanValue True)
    test "eval: (= 3.0 3)" (eval empty ex_expr_16) (Just $ BooleanValue True)
    test "eval: (cond ((= 3.0 3) 42) (#t 12))" (eval empty ex_expr_17) (Just $ IntegerValue 42)
    test "eval: (cond ((< 5 1) 10) (else 11))" (eval empty ex_expr_18) (Just $ IntegerValue 11)
    test "eval: (pair (+ 32 14) 3.14)" (eval empty ex_expr_19) (Just $ PairValue (IntegerValue 46) (DoubleValue 3.14))
    test "eval: (left (pair (+ 32 14) 3.14))" (eval empty ex_expr_20) (Just $ (IntegerValue 46))
    test "eval: (right (pair (+ 32 14) 3.14))" (eval empty ex_expr_21) (Just $ (DoubleValue 3.14))
    test "eval: (left 42)" (eval empty ex_expr_22) Nothing
    test "eval: (integer? 42)" (eval empty ex_expr_23) (Just $ BooleanValue True)
    test "eval: (integer? 3.14)" (eval empty ex_expr_24) (Just $ BooleanValue False)
    test "eval: (real? 42)" (eval empty ex_expr_25) (Just $ BooleanValue False)
    test "eval: (real? 3.14)" (eval empty ex_expr_26) (Just $ BooleanValue True)
    test "eval: (number? 42)" (eval empty ex_expr_27) (Just $ BooleanValue True)
    test "eval: (number? 3.14)" (eval empty ex_expr_28) (Just $ BooleanValue True)
    test "eval: (number? #f)" (eval empty ex_expr_29) (Just $ BooleanValue False)
    test "eval: (pair? (pair (+ 32 14) 3.14))" (eval empty ex_expr_30) (Just $ BooleanValue True)
    test "eval: (pair? (left (pair (+ 32 14) 3.14)))" (eval empty ex_expr_31) (Just $ BooleanValue False)
    test "eval: (boolean? #t)" (eval empty ex_expr_32) (Just $ BooleanValue True)
    test "eval: (boolean? 42)" (eval empty ex_expr_33) (Just $ BooleanValue False)
    test "eval: (boolean? (left 42))" (eval empty (IsBoolean ex_expr_22)) Nothing

-- showing that if can be expressed as cond
test_eval_if = do
  test "eval: (if #t 42 0)"
    (eval empty (If (Boolean True) (Integer 42) (Integer 0)))
    (eval empty (Cond $ WithElse [(ClauseIf (Boolean True) (Integer 42))] (Else (Integer 0))))
  test "eval: (if #f 42 0)"
    (eval empty (If (Boolean False) (Integer 42) (Integer 0)))
    (eval empty (Cond $ WithElse [(ClauseIf (Boolean False) (Integer 42))] (Else (Integer 0))))
  test "eval: (if #f 1 (if #t 2))"
    (eval empty (If (Boolean False) (Integer 1) (If (Boolean True) (Integer 2) (Integer 3))))
    (eval empty (Cond $ WithElse [(ClauseIf (Boolean False) (Integer 1)), (ClauseIf (Boolean True) (Integer 2))] (Else (Integer 3))))
  

-- |Substitutes the given value for the given variable in the given expression.
subst :: Variable -> Value -> Expr -> Expr
subst _ _ (Integer n) = Integer n
subst _ _ (Real n) = Real n
subst _ _ (Boolean b) = Boolean b
subst x (IntegerValue v) (Var y) | x == y = Integer v
                                 | otherwise = Var y
subst x (DoubleValue v) (Var y) | x == y = Real v
                                | otherwise = Var y
subst x (BooleanValue v) (Var y) | x == y = Boolean v
                                 | otherwise = Var y
subst x v (Add e1 e2) = Add (subst x v e1) (subst x v e2)
subst x v (Sub e1 e2) = Sub (subst x v e1) (subst x v e2)
subst x v (Mul e1 e2) = Mul (subst x v e1) (subst x v e2)
subst x v (Div e1 e2) = Div (subst x v e1) (subst x v e2)
subst x v (Let y e1 e2) | x == y = Let y (subst x v e1) e2
                        | otherwise = Let y (subst x v e1) (subst x v e2)
subst x v (If c e1 e2) = If (subst x v c) (subst x v e1) (subst x v e2)
subst x v (And e1 e2) = And (subst x v e1) (subst x v e2)
subst x v (Or e1 e2) = Or (subst x v e1) (subst x v e2)
subst x v (Not e) = Not (subst x v e)
subst x v (GreaterThan e1 e2) = GreaterThan (subst x v e1) (subst x v e2)
subst x v (LessThan e1 e2) = LessThan (subst x v e1) (subst x v e2)
subst x v (Equals e1 e2) = Equals (subst x v e1) (subst x v e2)
subst x v (Cond conds) =  
  case conds of
    (WithoutElse clauses) -> Cond $ WithoutElse (substClauses x v clauses)      
    (WithElse clauses (Else e1)) -> Cond $ WithElse (substClauses x v clauses) (Else (subst x v e1))
subst x v (Pair e1 e2) = Pair (subst x v e1) (subst x v e2)
subst x v (PLeft e) = PLeft (subst x v e)
subst x v (PRight e) = PRight (subst x v e)
subst x v (IsInteger e) = IsInteger (subst x v e)
subst x v (IsReal e) = IsReal (subst x v e)
subst x v (IsNumber e) = IsNumber (subst x v e)
subst x v (IsPair e) = IsPair (subst x v e)
subst x v (IsBoolean e) = IsBoolean (subst x v e)
subst x v (Call f e es) = Call f (subst x v e) (substList x v es)
  where substList x v [] = []
        substList x v (e:es) = [(subst x v e)] ++ (substList x v es)

substClauses :: Variable -> Value -> [Clause] -> [Clause]
substClauses x v [] = []
substClauses x v ((ClauseIf e1 e2):xs) = (ClauseIf (subst x v e1) (subst x v e2)):(substClauses x v xs)

substValues :: [Variable] -> [Value] -> Expr -> Expr
substValues [] _ e = e
substValues _ [] e = e -- will eventually error because function might use var that is not passed in
substValues (a:as) (v:vs) e = (substValues as vs (subst a v e))
               
test_subst = do
    test "subst x 42 x" (subst "x" (IntegerValue 42) (Var "x")) (Integer 42)
    test "subst x 42 y" (subst "x" (IntegerValue 42) (Var "y")) (Var "y")
    test "subst x 42 (* x 13)" (subst "x" (IntegerValue 42) (Mul (Var "x") (Integer 13))) (Mul (Integer 42) (Integer 13))
    test "subst x 42 (let (x (+ x 1)) (+ x 10))"
      (subst "x" (IntegerValue 42) (Let "x" (Add (Var "x") (Integer 1)) (Add (Var "x") (Integer 10))))
      (Let "x" (Add (Integer 42) (Integer 1)) (Add (Var "x") (Integer 10)))
    test "subst x 42 (let (y (+ x 1)) (+ y x))"
      (subst "x" (IntegerValue 42) (Let "y" (Add (Var "x") (Integer 1)) (Add (Var "y") (Var "x"))))
      (Let "y" (Add (Integer 42) (Integer 1)) (Add (Var "y") (Integer 42)))
    test "subst x 42 (pair x x)"
      (subst "x" (IntegerValue 42) (Pair (Var "x") (Var "x")))
      (Pair (Integer 42) (Integer 42))

test_substValues = do
  test "substValues [x, y] [42, 13] (+ x y)"
    (substValues ["x", "y"] [(IntegerValue 42), (IntegerValue 13)] (Add (Var "x") (Var "y")))
    (Add (Integer 42) (Integer 13))
  test "substValues [] [] 42"
    (substValues [] [] (Integer 42))
    (Integer 42)
  test "substValues [x] [] x"
    (substValues ["x"] [] (Var "x"))
    (Var "x")
  test "substValues [x] [42, 13] x"
    (substValues ["x"] [(IntegerValue 42), (IntegerValue 13)] (Var "x"))
    (Integer 42)

-- |Evaluates the final expression in a Program, taking into context the global definitions
evalProgram :: Program -> Maybe Value
evalProgram (Program globals ex) = eval (env empty globals) ex
  where
    env :: GlobalEnv -> [GlobalDef] -> GlobalEnv
    env m [] = m
    env m (gdf:defs) = env newM defs
      where newM = case gdf of
                     (Define v ex) ->
                       case get m v of
                         Just (Define v definedV) ->
                           case eval m definedV of
                             Just mapValue -> set m v (Define v (subst v mapValue ex))
                             _ -> set m v (Define v ex)
                         _ -> set m v (Define v ex)
                     (Defun f args ex) -> set m f (Defun f args ex)

test_evalProgram = do
  test "evalProgram [42]"
    (evalProgram ex_program_1)
    (Just $ IntegerValue 42)
  test "evalProgram [ \n (define x 42 ) \n x ]"
    (evalProgram ex_program_2)
    (Just $ IntegerValue 42)
  test "evalProgram [ \n (define x 42 ) \n (let (x 3.14) x)]"
    (evalProgram ex_program_3)
    (Just $ DoubleValue 3.14)
  test "evalProgram [ \n (define x 42) \n (define x (+ 10 x)) \n x ]"
    (evalProgram ex_program_4)
    (Just $ IntegerValue 52)
  test "evalProgram [ \n (define x 42) \n (defun double (y) (* 2 y) \n (double x) ]"
    (evalProgram ex_program_5)
    (Just $ IntegerValue 84)
  test "evalProgram [ \n (define x 42) \n (defun add (a b) (+ a b)) \n (add x x) ]"
    (evalProgram ex_program_6)
    (Just $ IntegerValue 84)


-- |Run the given protoScheme s-expressions as a program,
--  returning an s-expression representation of the result.
runSExpression :: [S.Expr] -> Maybe S.Expr
runSExpression se =
    case evalProgram (programFromSExpression se) of
         Just v -> Just (valueToSExpression v)
         Nothing -> Nothing

test_runSExpression = do
    test "run: [ \n (+ 1 2) \n ]" 
        (runSExpression [ S.List [S.Symbol "+", S.Integer 1, S.Integer 2] ])
        (Just $ S.Integer 3)
    test "run: [ \n (* (+ 25 13) (- 12 11)) \n ]"
        (runSExpression [ ex_sexpr_5 ])
        (Just $ S.Integer 38 )
    test "run: [ \n (/ 3.14 10) \n ]"
        (runSExpression [ S.List [S.Symbol "/", S.Real 3.14, S.Integer 10] ])
        (Just $ S.Real 0.314)
    test "run: [ \n (let (\"x\" 21) (+ \"x\" 23)) \n ]"
        (runSExpression [ ex_sexpr_6 ])
        (Just $ S.Integer 44)
    test "run: [ \n (define x 13) \n (defun f (x) (* x 10)) \n (let (x (+ x 25)) (f x)) \n ]"
        (runSExpression
         [
           S.List [S.Symbol "define", S.Symbol "x", S.Integer 13],
           S.List [S.Symbol "defun", S.Symbol "f", S.List [S.Symbol "x"], S.List [S.Symbol "*", S.Symbol "x", S.Integer 10]],
           S.List [S.Symbol "let", S.List [S.Symbol "x", S.List [S.Symbol "+", S.Symbol "x", S.Integer 25]],
                   (S.List [S.Symbol "f", S.Symbol "x"])]
         ])
        (Just $ S.Integer 380)
