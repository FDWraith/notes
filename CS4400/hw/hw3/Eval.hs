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

-- type EitherIntOrDouble = Either Integer Double

-- |Evaluates the given expression to a value.
eval :: Expr -> Maybe EitherIntOrDouble
eval (Integer i) = Just (Left i)
eval (Real r) = Just (Right r)
eval (Var _) = Nothing
eval (Add e1 e2) = 
    case eval e1 of
         Just (Left v1) -> 
            case eval e2 of
                 Just (Left v2) -> Just (Left (v1 + v2))
                 Just (Right v2) -> Just (Right ((fromInteger v1) + v2))
                 Nothing -> Nothing
         Just (Right v1) ->
            case eval e2 of
                 Just (Left v2) -> Just (Right (v1 + (fromInteger v2)))
                 Just (Right v2) -> Just (Right (v1 + v2))
                 Nothing -> Nothing
         Nothing -> Nothing
eval (Sub e1 e2) =
    case eval e1 of
         Just (Left v1) -> 
            case eval e2 of
                 Just (Left v2) -> Just (Left (v1 - v2))
                 Just (Right v2) -> Just (Right ((fromInteger v1) - v2))
                 Nothing -> Nothing
         Just (Right v1) ->
            case eval e2 of
                 Just (Left v2) -> Just (Right (v1 - (fromInteger v2)))
                 Just (Right v2) -> Just (Right (v1 - v2))
                 Nothing -> Nothing
         Nothing -> Nothing
eval (Mul e1 e2) =
    case eval e1 of
         Just (Left v1) -> 
            case eval e2 of
                 Just (Left v2) -> Just (Left (v1 * v2))
                 Just (Right v2) -> Just (Right ((fromInteger v1) * v2))
                 Nothing -> Nothing
         Just (Right v1) ->
            case eval e2 of
                 Just (Left v2) -> Just (Right (v1 * (fromInteger v2)))
                 Just (Right v2) -> Just (Right (v1 * v2))
                 Nothing -> Nothing
         Nothing -> Nothing
eval (Div e1 e2) =
    case eval e1 of
         Just (Left v1) -> 
            case eval e2 of
                 Just (Left v2) -> Just (Left (v1 `div` v2))
                 Just (Right v2) -> Just (Right ((fromInteger v1) / v2))
                 Nothing -> Nothing
         Just (Right v1) ->
            case eval e2 of
                 Just (Left v2) -> Just (Right (v1 / (fromInteger v2)))
                 Just (Right v2) -> Just (Right (v1 / v2))
                 Nothing -> Nothing
         Nothing -> Nothing
eval (Let x e1 e2) = 
    case eval e1 of
         Just v1 -> eval (subst x v1 e2)
         Nothing -> Nothing

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
    test "eval: (+ 12 30)" (eval (Add (Integer 12) (Integer 30))) (Just (Left 42))
    test "eval: (let (x (+1 2)) (* 4 x))" 
       (eval $ Let "x" (Add (Integer 1) (Integer 2)) (Mul (Integer 4) (Var "x")))
       (Just $ Left 12)
    test "eval: (+ 32 14)" (eval ex_expr_3) (Just $ Left 46)
    test "eval: (/ 15 5)" (eval ex_expr_4) (Just $ Left 3)
    test "eval: (* (+ 25 13) (- 12 11))" (eval ex_expr_5) (Just $ Left 38)
    test "eval: (let (\"x\" 21) (+ \"x\" 23))" (eval ex_expr_6) (Just $ Left 44)
    test "eval: (/ 3.14 10)" (eval (Div (Real 3.14) (Integer 10))) (Just $ Right 0.314)


-- |Substitutes the given value for the given variable in the given expression.
subst :: Variable -> EitherIntOrDouble -> Expr -> Expr
subst _ _ (Integer n) = Integer n
subst _ _ (Real n) = Real n
subst x (Left v) (Var y) | x == y = Integer v
                         | otherwise = Var y
subst x (Right v) (Var y) | x == y = Real v
                          | otherwise = Var y
subst x v (Add e1 e2) = Add (subst x v e1) (subst x v e2)
subst x v (Sub e1 e2) = Sub (subst x v e1) (subst x v e2)
subst x v (Mul e1 e2) = Mul (subst x v e1) (subst x v e2)
subst x v (Div e1 e2) = Div (subst x v e1) (subst x v e2)
subst x v (Let y e1 e2) | x == y = Let y (subst x v e1) e2
                        | otherwise = Let y (subst x v e1) (subst x v e2)


test_subst = do
    test "subst x 42 x" (subst "x" (Left 42) (Var "x")) (Integer 42)
    test "subst x 42 y" (subst "x" (Left 42) (Var "y")) (Var "y")
    test "subst x 42 (* x 13)" (subst "x" (Left 42) (Mul (Var "x") (Integer 13))) (Mul (Integer 42) (Integer 13))
    test "subst x 42 (let (x (+ x 1)) (+ x 10))"
      (subst "x" (Left 42) (Let "x" (Add (Var "x") (Integer 1)) (Add (Var "x") (Integer 10))))
      (Let "x" (Add (Integer 42) (Integer 1)) (Add (Var "x") (Integer 10)))
    test "subst x 42 (let (y (+ x 1)) (+ y x))"
      (subst "x" (Left 42) (Let "y" (Add (Var "x") (Integer 1)) (Add (Var "y") (Var "x"))))
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
