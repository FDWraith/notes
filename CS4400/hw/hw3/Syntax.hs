{-|
Module      : Syntax
Description : Abstract syntax of protoScheme
Copyright   : (c) Ferd, 2020
                  Kevin Zhang, 2020
Maintainer  : f.vesely@northeastern
              zhang.kevi@northeastern.edu

This module defines the abstract syntax of protoScheme and related functions.
-}
module Syntax where

import qualified SExpression as S

import SimpleTests (test)

-- |Variables are just strings
type Variable = String

type EitherIntOrDouble = Either Integer Double

{-
Kevin's Note:

Added EitherIntOrDouble for eval.

Changes made here are to add Real constructor. This
is because Expr is simply a representation of a value,
so we don't have to merge it with Integer.
-}
-- |protoScheme expressions
data Expr = Integer Integer
          | Real Double
          | Var Variable
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Variable Expr Expr
          deriving (Eq, Show)

-- |Parse an s-expression and convert it into a protoScheme expression.
fromSExpression :: S.Expr -> Expr
fromSExpression (S.Integer i) = Integer i
fromSExpression (S.Real r) = Real r
fromSExpression (S.List [S.Symbol "+", e1, e2]) = 
    Add (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "-", e1, e2]) = 
    Sub (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "*", e1, e2]) = 
    Mul (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "/", e1, e2]) = 
    Div (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "let", S.List [S.Symbol x, e1], e2]) =
    Let x (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.Symbol x) = Var x


ex_sexpr_1 = S.Integer 42
ex_sexpr_2 = S.Symbol "x"
ex_sexpr_3 = S.List [S.Symbol "+", S.Integer 32, S.Integer 14]
ex_sexpr_4 = S.List [S.Symbol "/", S.Integer 15, S.Integer 5]
ex_sexpr_5 = S.List [S.Symbol "*", S.List [S.Symbol "+", S.Integer 25, S.Integer 13], S.List [S.Symbol "-", S.Integer 12, S.Integer 11]]
ex_sexpr_6 = S.List [S.Symbol "let", S.List [S.Symbol "x", S.Integer 21], S.List [S.Symbol "+", S.Symbol "x", S.Integer 23]]
ex_sexpr_7 = S.Real 3.14

ex_expr_1 = Integer 42
ex_expr_2 = Var "x"
ex_expr_3 = Add (Integer 32) (Integer 14)
ex_expr_4 = Div (Integer 15) (Integer 5)
ex_expr_5 = Mul (Add (Integer 25) (Integer 13)) (Sub (Integer 12) (Integer 11))
ex_expr_6 = Let "x" (Integer 21) (Add (Var "x") (Integer 23))
ex_expr_7 = Real 3.14

test_fromSExpression = do
    test "fromSExpression 42" (fromSExpression ex_sexpr_1) ex_expr_1
    test "fromSExpression x" (fromSExpression ex_sexpr_2) ex_expr_2 
    test "fromSExpression (+ 32 14)" (fromSExpression ex_sexpr_3) ex_expr_3
    test "fromSExpression (/ 15 5)" (fromSExpression ex_sexpr_4) ex_expr_4
    test "fromSExpression (* (+ 25 13) (- 12 11))" (fromSExpression ex_sexpr_5) ex_expr_5
    test "fromSExpression (let (\"x\" 21) (+ \"x\" 23))" (fromSExpression ex_sexpr_6) ex_expr_6
    test "fromSExpression 3.14" (fromSExpression ex_sexpr_7) ex_expr_7

-- |Convert a protoScheme expression into its s-expression representation
toSExpression :: Expr -> S.Expr
toSExpression (Integer i) = S.Integer i
toSExpression (Real i) = S.Real i
toSExpression (Var v) = S.Symbol v
toSExpression (Add e1 e2) = S.List [S.Symbol "+", toSExpression e1, toSExpression e2]
toSExpression (Sub e1 e2) = S.List [S.Symbol "-", toSExpression e1, toSExpression e2]
toSExpression (Div e1 e2) = S.List [S.Symbol "/", toSExpression e1, toSExpression e2]
toSExpression (Mul e1 e2) = S.List [S.Symbol "*", toSExpression e1, toSExpression e2]
toSExpression (Let v e1 e2) = S.List [S.Symbol "let", S.List [S.Symbol v, toSExpression e1], toSExpression e2]

test_toSExpression = do
    test "toSExpression (+ 32 14)" 
        (toSExpression $ Add (Integer 32) (Integer 14))
        (S.List [S.Symbol "+", S.Integer 32, S.Integer 14])
    test "toSExpression 42" (toSExpression ex_expr_1) ex_sexpr_1
    test "toSExpression x" (toSExpression ex_expr_2) ex_sexpr_2
    test "toSExpression (+ 32 14)" (toSExpression ex_expr_3) ex_sexpr_3
    test "toSExpression (/ 15 5)" (toSExpression ex_expr_4) ex_sexpr_4
    test "toSExpression (* (+ 25 13) (- 12 11))" (toSExpression ex_expr_5) ex_sexpr_5
    test "toSExpression (let (\"x\" 21) (+ \"x\" 23))" (toSExpression ex_expr_6) ex_sexpr_6
    test "toSExpression 3.14" (toSExpression ex_expr_7) ex_sexpr_7

-- |Convert an evaluation result into s-expression
valueToSExpression :: EitherIntOrDouble -> S.Expr
valueToSExpression (Left i) = S.Integer i
valueToSExpression (Right r) = S.Real r

test_valueToSExpression = do
    test "toSExpression 42" 
        (valueToSExpression (Left 42))
        (S.Integer 42)
    test "toSExpression 3.14"
        (valueToSExpression (Right 3.14))
        (S.Real 3.14)


