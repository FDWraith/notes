{-|
Module      : Syntax
Description : Abstract syntax of protoScheme
Copyright   : (c) Ferd, 2020
                  Jack, 2020
Maintainer  : f.vesely@northeastern
              gelinas.j@northeastern.edu

This module defines the abstract syntax of protoScheme and related functions.
-}
module Syntax
    ( fromSExpression
    , programFromSExpression
    , toSExpression
    , valueToSExpression
    , integer
    , float
    , bool
    , Variable
    , Value (..)
    , GlobalDef (..)
    , Program (..)
    , Expr (..)
    , Clause
    , Syntax.allTests
    ) where

import qualified SExpression as S
import SimpleTestsColor (test, testSection)

-- |Variables are just strings
type Variable = String

-- |(Internal) Values
-- <Value> ::= <Integer>
--           | <Float>
--           | <Boolean>
--           | ( <Value> . <Value> )
data Value = Integer Integer
           | Float Double
           | Boolean Bool
           | PairV Value Value
           deriving (Eq, Show)

{-

  <Program> ::= (<GlobalDef>* <Expr>)

  <GlobalDef> ::= (defun <Variable> (<Variable>+) <Expr>)
                | (define <Variable> <Expr>)

  <Expr> ::= <Value>
           | <Variable>
           | (+ <Expr> <Expr>)
           | (- <Expr> <Expr>)
           | (* <Expr> <Expr>)
           | (/ <Expr> <Expr>)
           | (let (<Variable> <Expr>) <Expr>)
           | (if <Expr> <Expr> <Expr>)
           | (and <Expr> <Expr>)
           | (or <Expr> <Expr>)
           | (not <Expr>)
           | (< <Expr> <Expr>)
           | (> <Expr> <Expr>)
           | (= <Expr> <Expr>)
           | (cond (<Expr> <Expr>)*)
           | (cond (<Expr> <Expr>)* (else <Expr>))
           | (pair <Expr> <Expr>)
           | (left <Expr>)
           | (right <Expr>)
-}

-- |A _program_ is global definitions + expression
data Program = Program [GlobalDef] Expr
             deriving (Eq, Show)

-- |Global definitions
data GlobalDef = Defun Variable [Variable] Expr -- Assumption: [Variable] is non-empty
               | Define Variable Expr
               deriving (Eq, Show)

-- |protoScheme expressions
data Expr = Value Value
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
          | Lt Expr Expr
          | Gt Expr Expr
          | Eq Expr Expr
          | Cond [Clause] (Maybe Expr)
          | Pair Expr Expr
          | PLeft Expr
          | PRight Expr
          | Call Variable [Expr]
          | RealHuh Expr
          | IntegerHuh Expr
          | NumberHuh Expr
          | BooleanHuh Expr
          | PairHuh Expr
          deriving (Eq, Show)

-- |A clause is a condition expression with an expression
type Clause = (Expr, Expr)

-- |An integer as a protoScheme expression
integer :: Integer -> Expr
integer i = Value $ Integer i

-- |A double as a protoScheme float expression.
float :: Double -> Expr
float d = Value $ Float d

-- |A boolean as a protoScheme expression.
bool :: Bool -> Expr
bool b = Value $ Boolean b

-- |Parse a program s-expression
programFromSExpression :: S.Expr -> Program
programFromSExpression (S.List es) = go [] es
  where
    go defs [e] = Program (reverse defs) (fromSExpression e)
    go defs (e : es) = go (globalFromSExpr e : defs) es

-- |Parse an s-expression and convert it into a protoScheme expression.
fromSExpression :: S.Expr -> Expr
fromSExpression (S.Integer i) = Value $ Integer i
fromSExpression (S.Real r) = Value $ Float r
fromSExpression (S.Boolean b) = Value $ Boolean b
fromSExpression (S.Symbol v) = Var v
fromSExpression (S.List (S.Symbol s : es)) 
    | s `elem` keywords = fromKeyword s es
  where
    keywords = [ "+", "-", "*", "/", "<", ">", "=", "not", "and", "or"
               , "let", "if", "cond", "pair", "left", "right"
               , "real?", "integer?", "number?", "boolean?", "pair?"
               ]
fromSExpression (S.List (S.Symbol s : args@(_ : _))) = 
    Call s $ map fromSExpression args

-- |Convert a protoScheme expression into its s-expression representation
toSExpression :: Expr -> S.Expr
toSExpression (Value v) = valueToSExpression v
toSExpression (Var v) = S.Symbol v
toSExpression (Add e1 e2) = toSymbolApp "+" [e1, e2]
toSExpression (Sub e1 e2) = toSymbolApp "-" [e1, e2]
toSExpression (Mul e1 e2) = toSymbolApp "*" [e1, e2]
toSExpression (Div e1 e2) = toSymbolApp "/" [e1, e2]
toSExpression (Let v e1 e2) =
    S.List [ S.Symbol "let"
           , S.List [ S.Symbol v, toSExpression e1 ]
           , toSExpression e2
           ]
toSExpression (If e1 e2 e3) = toSymbolApp "if" [e1, e2, e3]
toSExpression (And e1 e2) = toSymbolApp "and" [e1, e2]
toSExpression (Or e1 e2) = toSymbolApp "or" [e1, e2]
toSExpression (Not e) = toSymbolApp "not" [e]
toSExpression (Lt e1 e2) = toSymbolApp "<" [e1, e2]
toSExpression (Gt e1 e2) = toSymbolApp ">" [e1, e2]
toSExpression (Eq e1 e2) = toSymbolApp "=" [e1, e2]
toSExpression (Cond clauses elseExpr) =
    S.List $ S.Symbol "cond" : map fromClause clauses ++ elseClause elseExpr
  where 
    elseClause (Just e) = [ S.List [S.Symbol "else", toSExpression e] ]
    elseClause Nothing = []

    fromClause (e1, e2) = S.List [toSExpression e1, toSExpression e2]
toSExpression (Pair e1 e2) = toSymbolApp "pair" [e1, e2]
toSExpression (PLeft e) = toSymbolApp "left" [e]
toSExpression (PRight e) = toSymbolApp "right" [e]
toSExpression (Call f es) = toSymbolApp f es
toSExpression (RealHuh e) = toSymbolApp "real?" [e]
toSExpression (IntegerHuh e) = toSymbolApp "integer?" [e]
toSExpression (NumberHuh e) = toSymbolApp "number?" [e]
toSExpression (BooleanHuh e) = toSymbolApp "boolean?" [e]
toSExpression (PairHuh e) = toSymbolApp "pair?" [e]

-- |Convert an evaluation result into s-expression
valueToSExpression :: Value -> S.Expr
valueToSExpression (Integer i) = S.Integer i
valueToSExpression (Float f) = S.Real f
valueToSExpression (Boolean b) = S.Boolean b
valueToSExpression (PairV v1 v2) = 
    S.Dotted (valueToSExpression v1) (valueToSExpression v2)


-- Helpers

-- |Convert a global definition s-expression
globalFromSExpr :: S.Expr -> GlobalDef
globalFromSExpr (S.List [ S.Symbol "defun", S.Symbol fname, S.List args, body]) 
    | not $ null args = 
        Defun fname (map unSymbol args) $ fromSExpression body
  where
    unSymbol (S.Symbol s) = s
globalFromSExpr (S.List [ S.Symbol "define", S.Symbol var, expr ]) =
    Define var $ fromSExpression expr

-- |Convert a keyword symbol application
fromKeyword :: String -> [S.Expr] -> Expr
-- unary operations
fromKeyword s [e] | s `elem` unOps =
    constrOfUnOp s $ fromSExpression e
  where
    unOps :: [String]
    unOps = [ "not", "left", "right"
            , "real?", "integer?", "number?", "boolean?", "pair?" 
            ]
    constrOfUnOp :: String -> (Expr -> Expr)
    constrOfUnOp s = case s of
        "not" -> Not
        "left" -> PLeft
        "right" -> PRight
        "real?" -> RealHuh
        "integer?" -> IntegerHuh
        "number?" -> NumberHuh
        "boolean?" -> BooleanHuh
        "pair?" -> PairHuh
        _ -> error "Invalid unOp"

-- binary operations
fromKeyword s [e1, e2] | s `elem` binOps=
    fromSExpression2 (constrOfBinOp s) e1 e2
  where
    binOps :: [String]
    binOps = [ "+", "-", "*", "/", "and", "or", ">", "<", "=", "pair" ]
    constrOfBinOp :: String -> (Expr -> Expr -> Expr)
    constrOfBinOp s = case s of
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div
        "and" -> And
        "or" -> Or
        ">" -> Gt
        "<" -> Lt
        "=" -> Eq
        "pair" -> Pair
        _ -> error "Invalid binOp"

-- others
fromKeyword "let" [S.List [S.Symbol v, e1], e2] =
    fromSExpression2 (Let v) e1 e2
fromKeyword "if" [e1, e2, e3] =
    fromSExpression3 If e1 e2 e3
fromKeyword "cond" es 
    | not (null es) && isElse (last es) = 
        Cond (map toClause $ init es) (Just $ elseClause $ last es)
    | otherwise =
        Cond (map toClause es) Nothing
  where
    toClause :: S.Expr -> Clause
    toClause (S.List [e1, e2]) = (fromSExpression e1, fromSExpression e2)
    toClause _ = error "Invalid clause"

    isElse :: S.Expr -> Bool
    isElse (S.List [S.Symbol "else", _]) = True
    isElse _ = False

    elseClause :: S.Expr -> Expr
    elseClause (S.List [S.Symbol "else", e]) = fromSExpression e
    elseClause _ = error "Invalid else clause"


-- |Convert two s-expressions and apply a binary constructor.
fromSExpression2 :: (Expr -> Expr -> Expr) -> S.Expr -> S.Expr -> Expr
fromSExpression2 c e1 e2 = c (fromSExpression e1) (fromSExpression e2)

-- |Convert three s-expressions and apply a ternary constructor.
fromSExpression3 
    :: (Expr -> Expr -> Expr -> Expr) 
    -> S.Expr -> S.Expr -> S.Expr -> Expr
fromSExpression3 c e1 = fromSExpression2 (c $ fromSExpression e1)


-- |Convert a symbol application to an s-expression.
toSymbolApp :: String -> [Expr] -> S.Expr
toSymbolApp s es = S.List (S.Symbol s : map toSExpression es)


-- Tests

test_programFromSExpression = do
    test "(42)" 
        (programFromSExpression $ S.List [ S.Integer 42 ])
        (Program [] (integer 42))
    test "(x)" 
        (programFromSExpression $ S.List [ S.Symbol "x" ])
        (Program [] (Var "x"))
    test "((define x 42) (+ x x))"
        (programFromSExpression $
            S.List [ S.List [ S.Symbol "define"
                            , S.Symbol "x"
                            , S.Integer 42
                            ]
                   , S.List [ S.Symbol "+"
                            , S.Symbol "x"
                            , S.Symbol "x"
                            ]
                   ])
        (Program [ Define "x" (integer 42) ] (Add (Var "x") (Var "x")))
    test "((define x 42) (defun f (x y) (+ x y)) (+ (f x 0) (f x 1)))"
        (programFromSExpression $
            S.List [ S.List [ S.Symbol "define"
                            , S.Symbol "x"
                            , S.Integer 42
                            ]
                   , S.List [ S.Symbol "defun"
                            , S.Symbol "f"
                            , S.List [ S.Symbol "x", S.Symbol "y" ]
                            , S.List [ S.Symbol "+"
                                     , S.Symbol "x"
                                     , S.Symbol "y"
                                     ]
                            ]
                   , S.List [ S.Symbol "+"
                            , S.List [ S.Symbol "f", S.Symbol "x", S.Integer 0 ]
                            , S.List [ S.Symbol "f", S.Symbol "x", S.Integer 1 ]
                            ]
                   ])
        (Program 
            [ Define "x" (integer 42) 
            , Defun "f" ["x", "y"] (Add (Var "x") (Var "y"))
            ] 
            (Add (Call "f" [Var "x", integer 0]) 
                 (Call "f" [Var "x", integer 1])))

test_fromSExpression = do
    test "fromSExpression 42" (fromSExpression $ S.Integer 42) (Value (Integer 42))
    test "fromSExpression y" (fromSExpression $ S.Symbol "y") (Var "y")
    test "fromExpression (let (x 42) (+ x x))" 
        (fromSExpression $ S.List 
            [ S.Symbol "let"
            , S.List [S.Symbol "x", S.Integer 42]
            , S.List [S.Symbol "+", S.Symbol "x", S.Symbol "x"
            ]])
        (Let "x" (integer 42) (Add (Var "x") (Var "x")))
    test "fromSExpression (- 12 32)"
        (fromSExpression $ S.List [ S.Symbol "-", S.Integer 12, S.Integer 32 ])
        (Sub (integer 12) (integer 32))
    test "fromSExpression (* 12 (/ 32 0))"
        (fromSExpression $ S.List 
            [ S.Symbol "*"
            , S.Integer 12
            , S.List [S.Symbol "/", S.Integer 32, S.Integer 0]])
        (Mul (integer 12) (Div (integer 32) (integer 0)))
    test "fromSExpression true" 
        (fromSExpression $ S.Boolean True) 
        (Value $ Boolean True)
    test "fromExpression (if (< 0 1) 42 3.14)" 
        (fromSExpression $ S.List 
            [ S.Symbol "if"
            , S.List [S.Symbol "<", S.Integer 0, S.Integer 1]
            , S.Integer 42
            , S.Real 3.14
            ])
        (If (Lt (integer 0) (integer 1)) (integer 42) (float 3.14))
    test "fromSExpression (and #t #f)" 
        (fromSExpression $ S.List 
            [ S.Symbol "and"
            , S.Boolean True
            , S.Boolean False
            ])
        (And (bool True) (bool False))
    test "fromSExpression (or (> 0 1) (< 0 1))" 
        (fromSExpression $ S.List 
            [ S.Symbol "or"
            , S.List [S.Symbol ">", S.Integer 0, S.Integer 1]
            , S.List [S.Symbol "<", S.Integer 0, S.Integer 1]
            ])
        (Or (Gt (integer 0) (integer 1)) (Lt (integer 0) (integer 1)))
    test "fromSExpression (not (= 42 #t))" 
        (fromSExpression $ S.List 
            [ S.Symbol "not"
            , S.List [S.Symbol "=", S.Integer 42, S.Boolean True ]
            ])
        (Not (Eq (integer 42) (bool True)))
    test "fromSExpression (cond )" 
        (fromSExpression $ S.List [S.Symbol "cond"]) 
        (Cond [] Nothing)
    test "fromSExpression (cond (else 42))" 
       (fromSExpression $ S.List [ S.Symbol "cond", S.List [S.Symbol "else", S.Integer 42 ]]) 
       (Cond [] (Just $ integer 42))
    test "fromSExpression (cond ((= x 4) 42) ((> 3 0) 4) (#t #f))" 
        (fromSExpression $ S.List
            [ S.Symbol "cond"
            , S.List [ S.List [ S.Symbol "=", S.Symbol "x", S.Integer 4]
                     , S.Integer 42
                     ]
            , S.List [ S.List [ S.Symbol ">", S.Integer 3, S.Integer 0]
                     , S.Integer 4
                     ]
            , S.List [ S.Boolean True, S.Boolean False ]
            ])
        (Cond 
            [ (Eq (Var "x") (integer 4), integer 42)
            , (Gt (integer 3) (integer 0), integer 4)
            , (bool True, bool False)
            ]
            Nothing)
    test "fromSExpression (cond ((= x 4) 42) (else #f))" 
        (fromSExpression $ S.List
            [ S.Symbol "cond"
            , S.List [ S.List [ S.Symbol "=", S.Symbol "x", S.Integer 4]
                     , S.Integer 42
                     ]
            , S.List [ S.Symbol "else", S.Boolean False ]
            ])
        (Cond [(Eq (Var "x") (integer 4), integer 42)] (Just $ bool False))
    test "fromSExpression (pair 1 #f)"
        (fromSExpression $ 
            S.List [ S.Symbol "pair", S.Integer 1, S.Boolean False ])
        (Pair (integer 1) (bool False))
    test "fromSExpression (pair (if (not #t) 1 42) (pair 2 3))"
        (fromSExpression $ 
            S.List [ S.Symbol "pair"
                   , S.List [ S.Symbol "if"
                            , S.List [ S.Symbol "not", S.Boolean True ]
                            , S.Integer 1
                            , S.Integer 42 ]
                   , S.List [ S.Symbol "pair"
                            , S.Integer 2
                            , S.Integer 3 
                            ]
                   ])
        (Pair (If (Not $ bool True) (integer 1) (integer 42))
              (Pair (integer 2) (integer 3)))
    test "fromSExpression (left (pair 1 2))"
        (fromSExpression $ 
            S.List [ S.Symbol "left"
                   , S.List [ S.Symbol "pair"
                            , S.Integer 1
                            , S.Integer 2
                            ]
                   ])
        (PLeft (Pair (integer 1) (integer 2)))
    test "fromSExpression (right (if 42 (pair #t #f) (+ 12 13)))"
        (fromSExpression $
            S.List [ S.Symbol "right"
                   , S.List [ S.Symbol "if"
                            , S.Integer 42
                            , S.List [ S.Symbol "pair"
                                     , S.Boolean True
                                     , S.Boolean False
                                     ]
                            , S.List [ S.Symbol "+"
                                     , S.Integer 12
                                     , S.Integer 13
                                     ]
                            ]
                   ])
        (PRight (If (integer 42) 
                    (Pair (bool True) (bool False)) 
                    (Add (integer 12) (integer 13))))
    test "fromSExpression (foo 1 b 3 d)"
        (fromSExpression $
            S.List [ S.Symbol "foo"
                   , S.Integer 1
                   , S.Symbol "b"
                   , S.Integer 3
                   , S.Symbol "d"
                   ])
        (Call "foo" [integer 1, Var "b", integer 3, Var "d"])



test_toSExpression = do
    test "toSExpression bool" 
        (toSExpression $ bool True)
        (S.Boolean True)
    test "toSExpression integer" 
        (toSExpression $ integer 42)
        (S.Integer 42)
    test "toSExpression real" 
        (toSExpression $ float 12.4)
        (S.Real 12.4)
    test "toSExpression variable" 
        (toSExpression $ Var "x")
        (S.Symbol "x")
    test "toSExpression (+ (- 12 1) (* 10 (/ 12 4)))"
        (toSExpression $ 
            Add (Sub (integer 12) (integer 1))
                (Mul (integer 10)
                     (Div (integer 12) (integer 4))))
        (S.List [ S.Symbol "+"
                , S.List [ S.Symbol "-"
                         , S.Integer 12
                         , S.Integer 1
                         ]
                , S.List [ S.Symbol "*"
                         , S.Integer 10
                         , S.List [ S.Symbol "/"
                                  , S.Integer 12
                                  , S.Integer 4
                                  ]
                         ]
                ])
    test "toSExpression (let (x 42) x)" 
        (toSExpression $ Let "x" (integer 42) (Var "x"))
        (S.List [ S.Symbol "let"
                , S.List [ S.Symbol "x", S.Integer 42 ]
                , S.Symbol "x"
                ])
    test "toSExpression (if x y z)" 
        (toSExpression $ If (Var "x") (Var "y") (Var "z"))
        (S.List [ S.Symbol "if"
                , S.Symbol "x"
                , S.Symbol "y"
                , S.Symbol "z"
                ])
                  
    test "toSExpression (and (or (< 1 2) (> x y)) (not (= x 2)))"
        (toSExpression $
            And (Or (Lt (integer 1) (integer 2))
                    (Gt (Var "x") (Var "y")))
                (Not (Eq (Var "x") (integer 2))))
        (S.List [ S.Symbol "and"
                , S.List [ S.Symbol "or"
                         , S.List [ S.Symbol "<"
                                  , S.Integer 1
                                  , S.Integer 2
                                  ]
                         , S.List [ S.Symbol ">"
                                  , S.Symbol "x"
                                  , S.Symbol "y"
                                  ]
                         ]
                , S.List [ S.Symbol "not"
                         , S.List [ S.Symbol "="
                                  , S.Symbol "x"
                                  , S.Integer 2
                                  ]
                         ]
                ])
    test "toSExpression (cond ((= x y) (+ 1 2)) (#f 42))" 
        (toSExpression $ 
            Cond [ (Eq (Var "x") (Var "y"), Add (integer 1) (integer 2))
                 , (bool False, integer 42)
                 ] 
                 Nothing)
        (S.List [ S.Symbol "cond"
                , S.List [ S.List [ S.Symbol "=", S.Symbol "x", S.Symbol "y" ]
                         , S.List [ S.Symbol "+", S.Integer 1, S.Integer 2 ]
                         ]
                , S.List [ S.Boolean False
                         , S.Integer 42 
                         ]
                ])
    test "toSExpression (cond ((= x y) (+ 1 2)) (#f 42) (else 3.14)" 
        (toSExpression $ 
            Cond [ (Eq (Var "x") (Var "y"), Add (integer 1) (integer 2))
                 , (bool False, integer 42)
                 ] 
                 (Just $ float 3.14))
        (S.List [ S.Symbol "cond"
                , S.List [ S.List [ S.Symbol "=", S.Symbol "x", S.Symbol "y" ]
                         , S.List [ S.Symbol "+", S.Integer 1, S.Integer 2 ]
                         ]
                , S.List [ S.Boolean False
                         , S.Integer 42 
                         ]
                , S.List [ S.Symbol "else"
                         , S.Real 3.14
                         ]
                ])
    test "toSExpression (pair (left (pair 1 2)) (right (pair 3 4)))"
        (toSExpression $
            Pair (PLeft (Pair (integer 1) (integer 2)))
                 (PRight (Pair (integer 3) (integer 4))))
        (S.List [ S.Symbol "pair"
                , S.List [ S.Symbol "left"
                         , S.List [ S.Symbol "pair"
                                  , S.Integer 1
                                  , S.Integer 2
                                  ]
                         ]
                , S.List [ S.Symbol "right"
                         , S.List [ S.Symbol "pair"
                                  , S.Integer 3
                                  , S.Integer 4
                                  ]
                         ]
                ])


test_toValueSExpression = do
    test "integer -42"
        (valueToSExpression $ Integer $ -42)
        (S.Integer $ -42)
    test "float 3.14"
        (valueToSExpression $ Float 3.14)
        (S.Real 3.14)
    test "true"
        (valueToSExpression $ Boolean True)
        (S.Boolean True)
    test "false"
        (valueToSExpression $ Boolean False)
        (S.Boolean False)
    test "(#t . 3.14)"
        (valueToSExpression $ PairV (Boolean True) (Float 3.14))
        (S.Dotted (S.Boolean True) (S.Real 3.14))
    test "((#t . #f) . (1 . 2))"
        (valueToSExpression $ 
            PairV (PairV (Boolean True) (Boolean False)) 
                  (PairV (Integer 1) (Integer 2)))
        (S.Dotted (S.Dotted (S.Boolean True) (S.Boolean False))
                  (S.Dotted (S.Integer 1) (S.Integer 2)))

allTests :: IO ()                 
allTests = do
  testSection "programFromSExpression tests"
  test_programFromSExpression
  testSection "fromSExpression tests"
  test_fromSExpression
  testSection "toSExpression tests"
  test_toSExpression
  testSection "toValueSExpression tests"
  test_toValueSExpression

