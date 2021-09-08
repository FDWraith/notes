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

import Maps

import SimpleTests (test)

{-
<GlobalDef> ::= (defun <Variable> (<Variable> <Variable>*) <Expr>)
              | (define <Variable> <Expr>)

<Program> ::= <GlobalDef>* <Expr>

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
         | (pair <Expr> <Expr>)
         | (left <Expr>)
         | (right <Expr>)
         | (real? <Expr>)
         | (integer? <Expr>)
         | (number? <Expr>)
         | (pair? <Expr>)
         | (boolean? <Expr>)
         | (<Variable> <Expr> <Expr>*)
-}

-- |Variables are just strings
type Variable = String

data Value = IntegerValue Integer
           | DoubleValue Double
           | BooleanValue Bool
           | PairValue Value Value
           deriving (Eq, Show)

data Clause = ClauseIf Expr Expr
            deriving (Eq, Show)
data ElseClause = Else Expr
                deriving (Eq, Show)

data ListOfClauses = WithElse [Clause] ElseClause
                   | WithoutElse [Clause]
                   deriving (Eq, Show)

-- |program definitions
data GlobalDef = Defun Variable [Variable] Expr
               | Define Variable Expr
               deriving (Eq, Show)

data Program = Program [GlobalDef] Expr
             deriving (Eq, Show)

type GlobalEnv = Map Variable GlobalDef

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
          | Pair Expr Expr
          | PLeft Expr
          | PRight Expr
          | IsReal Expr
          | IsInteger Expr
          | IsNumber Expr
          | IsPair Expr
          | IsBoolean Expr
          | Call Variable Expr [Expr]
          deriving (Eq, Show)

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
fromSExpression (S.List [S.Symbol "pair", e1, e2]) =
    Pair (fromSExpression e1) (fromSExpression e2)
fromSExpression (S.List [S.Symbol "left", e1]) =
    PLeft (fromSExpression e1)
fromSExpression (S.List [S.Symbol "right", e1]) =
    PRight (fromSExpression e1)
fromSExpression (S.List [S.Symbol "integer?", e]) =
    IsInteger (fromSExpression e)
fromSExpression (S.List [S.Symbol "real?", e]) =
    IsReal (fromSExpression e)
fromSExpression (S.List [S.Symbol "number?", e]) =
    IsNumber (fromSExpression e)
fromSExpression (S.List [S.Symbol "pair?", e]) =
    IsPair (fromSExpression e)
fromSExpression (S.List [S.Symbol "boolean?", e]) =
    IsBoolean (fromSExpression e)
fromSExpression (S.List ((S.Symbol x):a1:arguments)) = Call x (fromSExpression a1) (fromSExpressionList arguments)
  where
    fromSExpressionList :: [S.Expr] -> [Expr]
    fromSExpressionList [] = []
    fromSExpressionList (a:as) = (fromSExpression a):(fromSExpressionList as)

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

-- |Form a protoscheme program from a list of s-expressions
programFromSExpression :: [S.Expr] -> Program
programFromSExpression (e:[]) = Program [] (fromSExpression e)
programFromSExpression (gd:prog) =
  case programFromSExpression prog of
    (Program [] e) -> Program gdf e
    (Program defs e) -> Program (gdf ++ defs) e 
  where
    gdf = case globalDefFromSExpression gd of
            Nothing -> []
            Just def -> [def]

-- |Form a global def from an s-expression. Ignores non-definitions 
globalDefFromSExpression :: S.Expr -> Maybe GlobalDef
globalDefFromSExpression (S.List [S.Symbol "define", S.Symbol v, e]) = Just $ Define v (fromSExpression e)
globalDefFromSExpression (S.List [S.Symbol "defun", S.Symbol f, S.List ((S.Symbol p):params), e]) =
  Just $ Defun f (p:(paramsFromSExpression params)) (fromSExpression e)
  where
    paramsFromSExpression :: [S.Expr] -> [Variable]
    paramsFromSExpression [] = []
    paramsFromSExpression ((S.Symbol p):params) = [p] ++ (paramsFromSExpression params)
globalDefFromSExpression _ = Nothing

test_fromConditionals = do
  test "fromConditionals []" (fromConditionals []) (WithoutElse [])
  test "fromConditionals [(#t 1)]"
    (fromConditionals [(S.List [S.Boolean True, S.Integer 1])])
    (WithoutElse [(ClauseIf (Boolean True) (Integer 1))])
  test "fromCondtionals [(#f 2) (else #t)]"
    (fromConditionals [(S.List [S.Boolean False, S.Integer 2]), (S.List [S.Symbol "else", S.Boolean True])])
    (WithElse [(ClauseIf (Boolean False) (Integer 2))] (Else (Boolean True)))

ex_globaldef_1 = Define "x" (Integer 42)
ex_globaldef_2 = Define "x" (Add (Integer 10) (Var "x"))
ex_globaldef_3 = Defun "double" ["y"] (Mul (Var "y") (Integer 2))
ex_globaldef_4 = Defun "add" ["a", "b"] (Add (Var "a") (Var "b"))

ex_globaldef_sexpr_1 = (S.List [S.Symbol "define", S.Symbol "x", S.Integer 42])
ex_globaldef_sexpr_2 = (S.List [S.Symbol "define", S.Symbol "x", (S.List [S.Symbol "+", S.Integer 10, S.Symbol "x"])])
ex_globaldef_sexpr_3 = (S.List [S.Symbol "defun", S.Symbol "double", S.List [S.Symbol "y"], (S.List [S.Symbol "*", S.Symbol "y", S.Integer 2])])
ex_globaldef_sexpr_4 = (S.List [S.Symbol "defun", S.Symbol "add", S.List [S.Symbol "a", S.Symbol "b"], S.List [S.Symbol "+", S.Symbol "a", S.Symbol "b"]])
ex_globaldef_sexpr_5 = (S.List [S.Symbol "function", S.Symbol "identity", S.List [S.Symbol "y"], S.List [S.Symbol "x"]]) -- malformed

ex_program_1 = Program [] (Integer 42)
ex_program_2 = Program [ex_globaldef_1] (Var "x")
ex_program_3 = Program [ex_globaldef_1] (Let "x" (Real 3.14) (Var "x"))
ex_program_5 = Program [ex_globaldef_1, ex_globaldef_3] (Call "double" (Var "x") [])
ex_program_6 = Program [ex_globaldef_1, ex_globaldef_4] (Call "add" (Var "x") [(Var "x")])

ex_program_sexpr_1 = [(S.Integer 42)]
ex_program_sexpr_1_5 = [ex_globaldef_sexpr_5, (S.Integer 42)]
ex_program_sexpr_2 = [ex_globaldef_sexpr_1, (S.Symbol "x")]
ex_program_sexpr_3 = [ex_globaldef_sexpr_1, (S.List [S.Symbol "let", S.List [S.Symbol "x", S.Real 3.14], S.Symbol "x"])]
ex_program_sexpr_5 = [ex_globaldef_sexpr_1, ex_globaldef_sexpr_3, (S.List [S.Symbol "double", S.Symbol "x"])]
ex_program_sexpr_6 = [ex_globaldef_sexpr_1, ex_globaldef_sexpr_4, (S.List [S.Symbol "add", S.Symbol "x", S.Symbol "x"])]

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
ex_sexpr_19 = S.List [S.Symbol "pair", ex_sexpr_3, ex_sexpr_7]
ex_sexpr_20 = S.List [S.Symbol "left", ex_sexpr_19]
ex_sexpr_21 = S.List [S.Symbol "right", ex_sexpr_19]
ex_sexpr_22 = S.List [S.Symbol "left", ex_sexpr_1] -- malformed
ex_sexpr_23 = S.List [S.Symbol "integer?", ex_sexpr_1]
ex_sexpr_24 = S.List [S.Symbol "integer?", ex_sexpr_7]
ex_sexpr_25 = S.List [S.Symbol "real?", ex_sexpr_1]
ex_sexpr_26 = S.List [S.Symbol "real?", ex_sexpr_7]
ex_sexpr_27 = S.List [S.Symbol "number?", ex_sexpr_1]
ex_sexpr_28 = S.List [S.Symbol "number?", ex_sexpr_7]
ex_sexpr_29 = S.List [S.Symbol "number?", S.Boolean False]
ex_sexpr_30 = S.List [S.Symbol "pair?", ex_sexpr_19]
ex_sexpr_31 = S.List [S.Symbol "pair?", ex_sexpr_20]
ex_sexpr_32 = S.List [S.Symbol "boolean?", S.Boolean True]
ex_sexpr_33 = S.List [S.Symbol "boolean?", ex_sexpr_1]
ex_sexpr_34 = S.List [S.Symbol "newFunc", ex_sexpr_1]

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
ex_expr_19 = Pair ex_expr_3 ex_expr_7
ex_expr_20 = PLeft ex_expr_19
ex_expr_21 = PRight ex_expr_19
ex_expr_22 = PLeft ex_expr_1
ex_expr_23 = IsInteger ex_expr_1
ex_expr_24 = IsInteger ex_expr_7
ex_expr_25 = IsReal ex_expr_1
ex_expr_26 = IsReal ex_expr_7
ex_expr_27 = IsNumber ex_expr_1
ex_expr_28 = IsNumber ex_expr_7
ex_expr_29 = IsNumber (Boolean False)
ex_expr_30 = IsPair ex_expr_19
ex_expr_31 = IsPair ex_expr_20
ex_expr_32 = IsBoolean (Boolean True)
ex_expr_33 = IsBoolean ex_expr_1
ex_expr_34 = Call "newFunc" (Integer 42) []

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
    test "fromSExpression (pair (+ 32 14) 3.14)" (fromSExpression ex_sexpr_19) ex_expr_19
    test "fromSExpression (left (pair (+ 32 14) 3.14))" (fromSExpression ex_sexpr_20) ex_expr_20
    test "fromSExpression (right (pair (+ 32 14) 3.14))" (fromSExpression ex_sexpr_21) ex_expr_21
    test "fromSExpression (left 42)" (fromSExpression ex_sexpr_22) ex_expr_22
    test "fromSExpression (integer? 42)" (fromSExpression ex_sexpr_23) ex_expr_23
    test "fromSExpression (integer? 3.14)" (fromSExpression ex_sexpr_24) ex_expr_24
    test "fromSExpression (real? 42)" (fromSExpression ex_sexpr_25) ex_expr_25
    test "fromSExpression (real? 3.14)" (fromSExpression ex_sexpr_26) ex_expr_26
    test "fromSExpression (number? 42)" (fromSExpression ex_sexpr_27) ex_expr_27
    test "fromSExpression (number? 3.14)" (fromSExpression ex_sexpr_28) ex_expr_28
    test "fromSExpression (number? #f)" (fromSExpression ex_sexpr_29) ex_expr_29
    test "fromSExpression (pair? (pair (+ 32 14) 3.14))" (fromSExpression ex_sexpr_30) ex_expr_30
    test "fromSExpression (pair? (left (pair (+ 32 14) 3.14)))" (fromSExpression ex_sexpr_31) ex_expr_31
    test "fromSExpression (boolean? #t)" (fromSExpression ex_sexpr_32) ex_expr_32
    test "fromSExpression (boolean? 42)" (fromSExpression ex_sexpr_33) ex_expr_33
    test "fromSExpression (newFunc 42)" (fromSExpression ex_sexpr_34) ex_expr_34

test_globalDefFromSExpression = do
  test "globalDefFromSExpression (define x 42)" (globalDefFromSExpression ex_globaldef_sexpr_1) (Just ex_globaldef_1)
  test "globalDefFromSExpression (define x (+ 10 x))" (globalDefFromSExpression ex_globaldef_sexpr_2) (Just ex_globaldef_2)
  test "globalDefFromSExpression (defun double (y) (* y 2))" (globalDefFromSExpression ex_globaldef_sexpr_3) (Just ex_globaldef_3)
  test "globalDefFromSExpression (defun add (a b) (+ a b))" (globalDefFromSExpression ex_globaldef_sexpr_4) (Just ex_globaldef_4)
  test "globalDefFromSExpression (function identity (y) y)" (globalDefFromSExpression ex_globaldef_sexpr_5) Nothing

test_programFromSExpression = do
  test "programFromSExpression [42]" (programFromSExpression ex_program_sexpr_1) ex_program_1
  test "programFromSExpression [ \n (function identity (y) y) \n 42 ]"
       (programFromSExpression ex_program_sexpr_1_5) ex_program_1
  test "programFromSExpression [ \n (define x 42 ) \n x ]"
       (programFromSExpression ex_program_sexpr_2) ex_program_2
  test "programFromSExpression [ \n (define x 42 ) \n (let (x 3.14) x)]"
       (programFromSExpression ex_program_sexpr_3) ex_program_3
  test "programFromSExpression [ \n (define x 42) \n (defun double (y) (* 2 y) \n (double x) ]"
       (programFromSExpression ex_program_sexpr_5) ex_program_5
  test "programFromSExpression [ \n (define x 42) \n (defun add (a b) (+ a b)) \n (add x x) ]"
       (programFromSExpression ex_program_sexpr_6) ex_program_6
    
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
toSExpression (Pair e1 e2) = S.List [S.Symbol "pair", toSExpression e1, toSExpression e2]
toSExpression (PLeft e) = S.List [S.Symbol "left", toSExpression e]
toSExpression (PRight e) = S.List [S.Symbol "right", toSExpression e]
toSExpression (IsInteger e) = S.List [S.Symbol "integer?", toSExpression e]
toSExpression (IsReal e) = S.List [S.Symbol "real?", toSExpression e]
toSExpression (IsNumber e) = S.List [S.Symbol "number?", toSExpression e]
toSExpression (IsPair e) = S.List [S.Symbol "pair?", toSExpression e]
toSExpression (IsBoolean e) = S.List [S.Symbol "boolean?", toSExpression e]
toSExpression (Call f e es) = S.List ( [S.Symbol f, toSExpression e] ++ toSExpressionList es)
  where toSExpressionList [] = []
        toSExpressionList (x:xs) = [toSExpression e] ++ (toSExpressionList xs)

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
    test "toSExpression (pair (+ 32 14) 3.14)" (toSExpression ex_expr_19) ex_sexpr_19
    test "toSExpression (left (pair (+ 32 14) 3.14))" (toSExpression ex_expr_20) ex_sexpr_20
    test "toSExpression (right (pair (+ 32 14) 3.14))" (toSExpression ex_expr_21) ex_sexpr_21
    test "toSExpression (left 42)" (toSExpression ex_expr_22) ex_sexpr_22
    test "toSExpression (integer? 42)" (toSExpression ex_expr_23) ex_sexpr_23
    test "toSExpression (integer? 3.14)" (toSExpression ex_expr_24) ex_sexpr_24
    test "toSExpression (real? 42)" (toSExpression ex_expr_25) ex_sexpr_25
    test "toSExpression (real? 3.14)" (toSExpression ex_expr_26) ex_sexpr_26
    test "toSExpression (number? 42)" (toSExpression ex_expr_27) ex_sexpr_27
    test "toSExpression (number? 3.14)" (toSExpression ex_expr_28) ex_sexpr_28
    test "toSExpression (number? #f)" (toSExpression ex_expr_29) ex_sexpr_29
    test "toSExpression (pair? (pair (+ 32 14) 3.14))" (toSExpression ex_expr_30) ex_sexpr_30
    test "toSExpression (pair? (left (pair (+ 32 14) 3.14)))" (toSExpression ex_expr_31) ex_sexpr_31
    test "toSExpression (boolean? #t)" (toSExpression ex_expr_32) ex_sexpr_32
    test "toSExpression (boolean? 42)" (toSExpression ex_expr_33) ex_sexpr_33
    test "toSExpression (newFunc 42)" (toSExpression ex_expr_34) ex_sexpr_34

-- |Convert an evaluation result into s-expression
valueToSExpression :: Value -> S.Expr
valueToSExpression (IntegerValue i) = S.Integer i
valueToSExpression (DoubleValue r) = S.Real r
valueToSExpression (BooleanValue b) = S.Boolean b
valueToSExpression (PairValue v1 v2) = S.Dotted (valueToSExpression v1) (valueToSExpression v2)

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
    test "toSExpression (pair 42 (pair #f 3.14))"
        (valueToSExpression (PairValue (IntegerValue 42) (PairValue (BooleanValue False) (DoubleValue 3.14))))
        (S.Dotted (S.Integer 42) (S.Dotted (S.Boolean False) (S.Real 3.14)))


