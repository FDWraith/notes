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

import qualified SExpression as S

import SimpleTests (test)

-- |Evaluates the given expression to a value.
eval :: Expr -> Maybe Value
eval (Integer i) = Just (IntegerValue i)
eval (Real r) = Just (DoubleValue r)
eval (Boolean b) = Just (BooleanValue b)
eval (Var _) = Nothing
eval (Add e1 e2) = 
    case eval e1 of
         Just (IntegerValue v1) -> 
            case eval e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 + v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) + v2))
                 Just (BooleanValue v2) -> error "Addition with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 + (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 + v2))
                 Just (BooleanValue v2) -> error "Addition with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Addition with Booleans"
         Nothing -> Nothing
eval (Sub e1 e2) =
    case eval e1 of
         Just (IntegerValue v1) -> 
            case eval e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 - v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) - v2))
                 Just (BooleanValue v2) -> error "Subtraction with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 - (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 - v2))
                 Just (BooleanValue v2) -> error "Subtraction with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Subtraction with Booleans"
         Nothing -> Nothing
eval (Mul e1 e2) =
    case eval e1 of
         Just (IntegerValue v1) -> 
            case eval e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 * v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) * v2))
                 Just (BooleanValue v2) -> error "Multiplication with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 * (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 * v2))
                 Just (BooleanValue v2) -> error "Multiplication with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Multiplication with Booleans"
         Nothing -> Nothing
eval (Div e1 e2) =
    case eval e1 of
         Just (IntegerValue v1) -> 
            case eval e2 of
                 Just (IntegerValue v2) -> Just (IntegerValue (v1 `div` v2))
                 Just (DoubleValue v2) -> Just (DoubleValue ((fromInteger v1) / v2))
                 Just (BooleanValue v2) -> error "Division with Booleans"
                 Nothing -> Nothing
         Just (DoubleValue v1) ->
            case eval e2 of
                 Just (IntegerValue v2) -> Just (DoubleValue (v1 / (fromInteger v2)))
                 Just (DoubleValue v2) -> Just (DoubleValue (v1 / v2))
                 Just (BooleanValue v2) -> error "Division with Booleans"
                 Nothing -> Nothing
         Just (BooleanValue v1) -> error "Division with Booleans"
         Nothing -> Nothing
eval (Let x e1 e2) = 
    case eval e1 of
         Just v1 -> eval (subst x v1 e2)
         Nothing -> Nothing
eval (If cond e1 e2) =
    case eval cond of
         Just (BooleanValue True) -> eval e1
         Just (BooleanValue False) -> eval e2
         _ -> Nothing         
eval (And e1 e2) =
    case eval e1 of
         Just (BooleanValue True) ->
            case eval e2 of
              Just (BooleanValue v) -> Just $ BooleanValue v
              _ -> Nothing
         Just (BooleanValue False) -> Just $ BooleanValue False
         _ -> Nothing
eval (Or e1 e2) =
    case eval e1 of
         Just (BooleanValue True) -> Just $ BooleanValue True
         Just (BooleanValue False) ->
            case eval e2 of
               Just (BooleanValue v) -> Just $ BooleanValue v
               _ -> Nothing
         _ -> Nothing
eval (Not e) =
  case eval e of
    Just (BooleanValue True) -> Just $ BooleanValue False
    Just (BooleanValue False) -> Just $ BooleanValue True
    _ -> Nothing
eval (GreaterThan e1 e2) =
  case eval e1 of
    Just (IntegerValue v1) ->
      case eval e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 > v2)
        Just (DoubleValue v2) -> Just $ BooleanValue ((fromInteger v1) > v2)
        _ -> Nothing
    Just (DoubleValue v1) ->
      case eval e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 > (fromInteger v2))
        Just (DoubleValue v2) -> Just $ BooleanValue (v1 > v2)
    _ -> Nothing
eval (LessThan e1 e2) =
  case eval e1 of
    Just (IntegerValue v1) ->
      case eval e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 < v2)
        Just (DoubleValue v2) -> Just $ BooleanValue ((fromInteger v1) < v2)
        _ -> Nothing
    Just (DoubleValue v1) ->
      case eval e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 < (fromInteger v2))
        Just (DoubleValue v2) -> Just $ BooleanValue (v1 < v2)
    _ -> Nothing
eval (Equals e1 e2) =
  case eval e1 of
    Just (IntegerValue v1) ->
      case eval e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 == v2)
        Just (DoubleValue v2) -> Just $ BooleanValue ((fromInteger v1) == v2)
        _ -> Nothing
    Just (DoubleValue v1) ->
      case eval e2 of
        Just (IntegerValue v2) -> Just $ BooleanValue (v1 == (fromInteger v2))
        Just (DoubleValue v2) -> Just $ BooleanValue (v1 == v2)
    Just (BooleanValue v1) ->
      case eval e2 of
        Just (BooleanValue v2) -> Just $ BooleanValue (v1 == v2)
        _ -> Nothing
    Nothing -> Nothing
eval (Cond conds) =
  case conds of
    (WithoutElse []) -> Nothing
    (WithoutElse ((ClauseIf e1 e2):xs)) ->
      case eval e1 of
        Just (BooleanValue True) -> eval e2
        Just (BooleanValue False) -> eval (Cond (WithoutElse xs))
        _ -> Nothing
    (WithElse [] (Else e)) -> eval e
    (WithElse ((ClauseIf e1 e2):xs) el) ->
      case eval e1 of
        Just (BooleanValue True) -> eval e2
        Just (BooleanValue False) -> eval (Cond (WithElse xs el))
        _ -> Nothing
        
{-

Copied Over for convienence

ex_expr_1 = Integer 42
ex_expr_2 = Var "x"
ex_expr_3 = Add (Integer 32) (Integer 14)
ex_expr_4 = Div (Integer 15) (Integer 5)
ex_expr_5 = Mul (Add (Integer 25) (Integer 13)) (Sub (Integer 12) (Integer 11))
ex_expr_6 = Let "x" (Integer 21) (Add (Var "x") (Integer 23))
-}

test_eval = do
    test "eval: (+ 12 30)" (eval (Add (Integer 12) (Integer 30))) (Just (IntegerValue 42))
    test "eval: (let (x (+1 2)) (* 4 x))" 
       (eval $ Let "x" (Add (Integer 1) (Integer 2)) (Mul (Integer 4) (Var "x")))
       (Just $ IntegerValue 12)
    test "eval: (+ 32 14)" (eval ex_expr_3) (Just $ IntegerValue 46)
    test "eval: (/ 15 5)" (eval ex_expr_4) (Just $ IntegerValue 3)
    test "eval: (* (+ 25 13) (- 12 11))" (eval ex_expr_5) (Just $ IntegerValue 38)
    test "eval: (let (\"x\" 21) (+ \"x\" 23))" (eval ex_expr_6) (Just $ IntegerValue 44)
    test "eval: (/ 3.14 10)" (eval (Div (Real 3.14) (Integer 10))) (Just $ DoubleValue 0.314)
    test "eval: (if #t 42 0)" (eval ex_expr_8) (Just $ IntegerValue 42)
    test "eval: (and #f (/ 42 0))" (eval ex_expr_9) (Just $ BooleanValue False)
    test "eval: (and #t #t)" (eval (And (Boolean True) (Boolean True))) (Just $ BooleanValue True)
    test "eval: (or #t (/ 42 0))" (eval ex_expr_10) (Just $ BooleanValue True)
    test "eval: (or #f (not #t))" (eval (Or (Boolean False) ex_expr_11)) (Just $ BooleanValue False)
    test "eval: (> 42 10)" (eval ex_expr_12) (Just $ BooleanValue True)
    test "eval: (< 5 1)" (eval ex_expr_13) (Just $ BooleanValue False)
    test "eval: (= 10 10)" (eval ex_expr_14) (Just $ BooleanValue True)
    test "eval: (= (not #t) #f)" (eval ex_expr_15) (Just $ BooleanValue True)
    test "eval: (= 3.0 3)" (eval ex_expr_16) (Just $ BooleanValue True)
    test "eval: (cond ((= 3.0 3) 42) (#t 12))" (eval ex_expr_17) (Just $ IntegerValue 42)
    test "eval: (cond ((< 5 1) 10) (else 11))" (eval ex_expr_18) (Just $ IntegerValue 11)

-- showing that if can be expressed as cond
test_eval_if = do
  test "eval: (if #t 42 0)"
    (eval (If (Boolean True) (Integer 42) (Integer 0)))
    (eval (Cond $ WithElse [(ClauseIf (Boolean True) (Integer 42))] (Else (Integer 0))))
  test "eval: (if #f 42 0)"
    (eval (If (Boolean False) (Integer 42) (Integer 0)))
    (eval (Cond $ WithElse [(ClauseIf (Boolean False) (Integer 42))] (Else (Integer 0))))
  test "eval: (if #f 1 (if #t 2))"
    (eval (If (Boolean False) (Integer 1) (If (Boolean True) (Integer 2) (Integer 3))))
    (eval (Cond $ WithElse [(ClauseIf (Boolean False) (Integer 1)), (ClauseIf (Boolean True) (Integer 2))] (Else (Integer 3))))
  

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
    

-- |Run the given protoScheme s-expression, returning an s-expression 
-- representation of the result.
runSExpression :: S.Expr -> Maybe S.Expr
runSExpression se =
    case eval (fromSExpression se) of
         Just v -> Just (valueToSExpression v)
         Nothing -> Nothing

test_runSExpression = do
    test "run: (+ 1 2)" 
        (runSExpression $ S.List [S.Symbol "+", S.Integer 1, S.Integer 2])
        (Just $ S.Integer 3)
    test "run: (* (+ 25 13) (- 12 11))"
        (runSExpression ex_sexpr_5)
        (Just $ S.Integer 38)
    test "run: (/ 3.14 10)"
        (runSExpression $ S.List [S.Symbol "/", S.Real 3.14, S.Integer 10])
        (Just $ S.Real 0.314)
    test "run: (let (\"x\" 21) (+ \"x\" 23))"
        (runSExpression ex_sexpr_6)
        (Just $ S.Integer 44)
