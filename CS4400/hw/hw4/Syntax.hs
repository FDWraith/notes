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

data Value = IntegerValue Integer
           | DoubleValue Double
           | BooleanValue Bool
           deriving (Eq, Show)


{-

<Expr> ::= <Integer>
         | <Double>
         | <Variable>
         | <Bool>
         | (+ <Expr> <Expr>)
         | (- <Expr> <Expr>)
         | (* <Expr> <Expr>)
         | (/ <Expr> <Expr>)
         | (let (<Variable> <Expr>) <Expr>)
         | (if <Expr> <Expr> <Expr>)
         | (and <Expr> <Expr>)
         | (or <Expr> <Expr>)
         | (not <Expr)
         | (> <Expr> <Expr>)
         | (< <Expr> <Expr>)
         | (= <Expr> <Expr>)
         | (cond (<Expr> <Expr>)* )
         | (cond (<Expr> <Expr>)* (else <Expr>))

-}

data Clause = ClauseIf Expr Expr
            deriving (Eq, Show)
data ElseClause = Else Expr
                deriving (Eq, Show)

data ListOfClauses = WithElse [Clause] ElseClause
                   | WithoutElse [Clause]
                   deriving (Eq, Show)

-- |protoScheme expressions
data Expr = Integer Integer
          | Real Double
          | Boolean Bool
          | Var Variable
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Variable Expr Expr
          | If Expr Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | GreaterThan Expr Expr
          | LessThan Expr Expr
          | Equals Expr Expr
          | Cond ListOfClauses
          deriving (Eq, Show)


{-

(cond
 (#t ...)
 (#f ...)
 (else e)
)
-}

-- |Parse an s-expression and convert it into a protoScheme expression.
fromSExpression :: S.Expr -> Expr
fromSExpression (S.Integer i) = Integer i
fromSExpression (S.Real r) = Real r
fromSExpression (S.Boolean b) = Boolean b
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
fromSExpression (S.List [S.Symbol "if", c, e1, e2]) =
    If (fromSExpression c) (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "and", e1, e2]) =
    And (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "or", e1, e2]) =
    Or (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "not", e]) =
    Not (fromSExpression e)
fromSExpression (S.List [S.Symbol ">", e1, e2]) =
    GreaterThan (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "<", e1, e2]) =
    LessThan (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "=", e1, e2]) =
    Equals (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List (S.Symbol "cond":clauses)) =
    Cond (fromConditionals clauses)

-- |Parse a list of conditionals (in s-expression format) and convert it into a protoScheme ListOfClauses
fromConditionals :: [S.Expr] -> ListOfClauses
fromConditionals [] = WithoutElse []
fromConditionals ((S.List [S.Symbol "else", e]) : []) = WithElse [] (Else (fromSExpression e))
fromConditionals ((S.List [e1, e2]) : conds) =
  case fromConditionals conds of
    (WithoutElse clauses) -> (WithoutElse (c:clauses))
    (WithElse clauses el) -> (WithElse (c:clauses) el)
    where c = ClauseIf (fromSExpression e1) (fromSExpression e2)
fromConditionals (_ : conds) = fromConditionals conds

test_fromConditionals = do
  test "fromConditionals []" (fromConditionals []) (WithoutElse [])
  test "fromConditionals [(#t 1)]"
    (fromConditionals [(S.List [S.Boolean True, S.Integer 1])])
    (WithoutElse [(ClauseIf (Boolean True) (Integer 1))])
  test "fromCondtionals [(#f 2) (else #t)]"
    (fromConditionals [(S.List [S.Boolean False, S.Integer 2]), (S.List [S.Symbol "else", S.Boolean True])])
    (WithElse [(ClauseIf (Boolean False) (Integer 2))] (Else (Boolean True)))

ex_sexpr_1 = S.Integer 42
ex_sexpr_2 = S.Symbol "x"
ex_sexpr_3 = S.List [S.Symbol "+", S.Integer 32, S.Integer 14]
ex_sexpr_4 = S.List [S.Symbol "/", S.Integer 15, S.Integer 5]
ex_sexpr_5 = S.List [S.Symbol "*", S.List [S.Symbol "+", S.Integer 25, S.Integer 13], S.List [S.Symbol "-", S.Integer 12, S.Integer 11]]
ex_sexpr_6 = S.List [S.Symbol "let", S.List [S.Symbol "x", S.Integer 21], S.List [S.Symbol "+", S.Symbol "x", S.Integer 23]]
ex_sexpr_7 = S.Real 3.14
ex_sexpr_8 = S.List [S.Symbol "if", S.Boolean True, S.Integer 42, S.Integer 0]
ex_sexpr_9 = S.List [S.Symbol "and", S.Boolean False, S.List [S.Symbol "/", S.Integer 42, S.Integer 0]]
ex_sexpr_10 = S.List [S.Symbol "or", S.Boolean True, S.List [S.Symbol "/", S.Integer 42, S.Integer 0]]
ex_sexpr_11 = S.List [S.Symbol "not", S.Boolean True]
ex_sexpr_12 = S.List [S.Symbol ">", S.Integer 42, S.Integer 10]
ex_sexpr_13 = S.List [S.Symbol "<", S.Integer 5, S.Integer 1]
ex_sexpr_14 = S.List [S.Symbol "=", S.Integer 10, S.Integer 10]
ex_sexpr_15 = S.List [S.Symbol "=", ex_sexpr_11, S.Boolean False]
ex_sexpr_16 = S.List [S.Symbol "=", S.Real 3.0, S.Integer 3]
ex_sexpr_17 = S.List [S.Symbol "cond",
                       S.List [ex_sexpr_16, S.Integer 42],
                       S.List [S.Boolean True, S.Integer 12]
                     ]
ex_sexpr_18 = S.List [S.Symbol "cond",
                       S.List [ex_sexpr_13, S.Integer 10],
                       S.List [S.Symbol "else", S.Integer 11]
                     ]

ex_expr_1 = Integer 42
ex_expr_2 = Var "x"
ex_expr_3 = Add (Integer 32) (Integer 14)
ex_expr_4 = Div (Integer 15) (Integer 5)
ex_expr_5 = Mul (Add (Integer 25) (Integer 13)) (Sub (Integer 12) (Integer 11))
ex_expr_6 = Let "x" (Integer 21) (Add (Var "x") (Integer 23))
ex_expr_7 = Real 3.14
ex_expr_8 = If (Boolean True) (Integer 42) (Integer 0)
ex_expr_9 = And (Boolean False) (Div (Integer 42) (Integer 0))
ex_expr_10 = Or (Boolean True) (Div (Integer 42) (Integer 0))
ex_expr_11 = Not (Boolean True)
ex_expr_12 = GreaterThan (Integer 42) (Integer 10)
ex_expr_13 = LessThan (Integer 5) (Integer 1)
ex_expr_14 = Equals (Integer 10) (Integer 10)
ex_expr_15 = Equals ex_expr_11 (Boolean False)
ex_expr_16 = Equals (Real 3.0) (Integer 3)
ex_expr_17 = Cond $ WithoutElse [(ClauseIf ex_expr_16 (Integer 42)), (ClauseIf (Boolean True) (Integer 12))]
ex_expr_18 = Cond $ WithElse [(ClauseIf ex_expr_13 (Integer 10))] (Else (Integer 11))

test_fromSExpression = do
    test "fromSExpression 42" (fromSExpression ex_sexpr_1) ex_expr_1
    test "fromSExpression x" (fromSExpression ex_sexpr_2) ex_expr_2 
    test "fromSExpression (+ 32 14)" (fromSExpression ex_sexpr_3) ex_expr_3
    test "fromSExpression (/ 15 5)" (fromSExpression ex_sexpr_4) ex_expr_4
    test "fromSExpression (* (+ 25 13) (- 12 11))" (fromSExpression ex_sexpr_5) ex_expr_5
    test "fromSExpression (let (\"x\" 21) (+ \"x\" 23))" (fromSExpression ex_sexpr_6) ex_expr_6
    test "fromSExpression 3.14" (fromSExpression ex_sexpr_7) ex_expr_7
    test "fromSExpression (if #t 42 0)" (fromSExpression ex_sexpr_8) ex_expr_8
    test "fromSExpression (and #f (/ 42 0))" (fromSExpression ex_sexpr_9) ex_expr_9
    test "fromSExpression (or #t (/ 42 0))" (fromSExpression ex_sexpr_10) ex_expr_10
    test "fromSExpression (not #t)" (fromSExpression ex_sexpr_11) ex_expr_11
    test "fromSExpression (> 42 10)" (fromSExpression ex_sexpr_12) ex_expr_12
    test "fromSExpression (< 5 1)" (fromSExpression ex_sexpr_13) ex_expr_13
    test "fromSExpression (= 10 10)" (fromSExpression ex_sexpr_14) ex_expr_14
    test "fromSExpression (= (not #t) #f)" (fromSExpression ex_sexpr_15) ex_expr_15
    test "fromSExpression (= 3.0 3)" (fromSExpression ex_sexpr_16) ex_expr_16
    test "fromSExpression (cond ((= 3.0 3) 42) (#t 12))" (fromSExpression ex_sexpr_17) ex_expr_17
    test "fromSExpression (cond ((< 5 1) 10) (else 11))" (fromSExpression ex_sexpr_18) ex_expr_18

-- |Convert a protoScheme expression into its s-expression representation
toSExpression :: Expr -> S.Expr
toSExpression (Integer i) = S.Integer i
toSExpression (Real i) = S.Real i
toSExpression (Boolean b) = S.Boolean b
toSExpression (Var v) = S.Symbol v
toSExpression (Add e1 e2) = S.List [S.Symbol "+", toSExpression e1, toSExpression e2]
toSExpression (Sub e1 e2) = S.List [S.Symbol "-", toSExpression e1, toSExpression e2]
toSExpression (Div e1 e2) = S.List [S.Symbol "/", toSExpression e1, toSExpression e2]
toSExpression (Mul e1 e2) = S.List [S.Symbol "*", toSExpression e1, toSExpression e2]
toSExpression (Let v e1 e2) = S.List [S.Symbol "let", S.List [S.Symbol v, toSExpression e1], toSExpression e2]
toSExpression (If c e1 e2) = S.List [S.Symbol "if", toSExpression c, toSExpression e1, toSExpression e2]
toSExpression (And e1 e2) = S.List [S.Symbol "and", toSExpression e1, toSExpression e2]
toSExpression (Or e1 e2) = S.List [S.Symbol "or", toSExpression e1, toSExpression e2]
toSExpression (Not e) = S.List [S.Symbol "not", toSExpression e]
toSExpression (GreaterThan e1 e2) = S.List [S.Symbol ">", toSExpression e1, toSExpression e2]
toSExpression (LessThan e1 e2) = S.List [S.Symbol "<", toSExpression e1, toSExpression e2]
toSExpression (Equals e1 e2) = S.List [S.Symbol "=", toSExpression e1, toSExpression e2]
toSExpression (Cond loc) = S.List (S.Symbol "cond":(condsToSExpression loc))

-- |Convert a ListOfClauses to a s-expression representation
condsToSExpression :: ListOfClauses -> [S.Expr]
condsToSExpression (WithoutElse []) = []
condsToSExpression (WithoutElse ((ClauseIf e1 e2):loc)) = (S.List [toSExpression e1, toSExpression e2]):(condsToSExpression (WithoutElse loc))
condsToSExpression (WithElse [] (Else e)) = [S.List [S.Symbol "else", toSExpression e]]
condsToSExpression (WithElse ((ClauseIf e1 e2):loc) (Else e)) = (S.List [toSExpression e1, toSExpression e2]):(condsToSExpression (WithElse loc (Else e)))

test_condsToSExpression = do
  test "condsToSExpression WithoutElse []" (condsToSExpression (WithoutElse [])) []
  test "condsToSExpression WithElse [] (Else 1)"
    (condsToSExpression (WithElse [] (Else (Integer 1))))
    [(S.List [S.Symbol "else", S.Integer 1])]
  test "condsToSExpression [(#t 1)]"
    (condsToSExpression (WithoutElse [(ClauseIf (Boolean True) (Integer 1))]))
    [(S.List [S.Boolean True, S.Integer 1])]    
  test "condsToSExpression [(#f 2) (else #t)]"
    (condsToSExpression (WithElse [(ClauseIf (Boolean False) (Integer 2))] (Else (Boolean True))))
    [(S.List [S.Boolean False, S.Integer 2]), (S.List [S.Symbol "else", S.Boolean True])]
    

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
    test "toSExpression (if #t 42 0)" (toSExpression ex_expr_8) ex_sexpr_8
    test "toSExpression (and #f (/ 42 0))" (toSExpression ex_expr_9) ex_sexpr_9
    test "toSExpression (or #t (/ 42 0))" (toSExpression ex_expr_10) ex_sexpr_10
    test "toSExpression (not #t)" (toSExpression ex_expr_11) ex_sexpr_11
    test "toSExpression (> 42 10)" (toSExpression ex_expr_12) ex_sexpr_12
    test "toSExpression (< 5 1)" (toSExpression ex_expr_13) ex_sexpr_13
    test "toSExpression (= 10 10)" (toSExpression ex_expr_14) ex_sexpr_14
    test "toSExpression (= (not #t) #f)" (toSExpression ex_expr_15) ex_sexpr_15
    test "toSExpression (= 3.0 3)" (toSExpression ex_expr_16) ex_sexpr_16
    test "toSExpression (cond ((= 3.0 3) 42) (#t 12))" (toSExpression ex_expr_17) ex_sexpr_17
    test "toSExpression (cond ((< 5 1) 10) (else 11))" (toSExpression ex_expr_18) ex_sexpr_18

-- |Convert an evaluation result into s-expression
valueToSExpression :: Value -> S.Expr
valueToSExpression (IntegerValue i) = S.Integer i
valueToSExpression (DoubleValue r) = S.Real r
valueToSExpression (BooleanValue b) = S.Boolean b

test_valueToSExpression = do
    test "toSExpression 42" 
        (valueToSExpression (IntegerValue 42))
        (S.Integer 42)
    test "toSExpression 3.14"
        (valueToSExpression (DoubleValue 3.14))
        (S.Real 3.14)
    test "toSExpression #f"
        (valueToSExpression (BooleanValue False))
        (S.Boolean False)


