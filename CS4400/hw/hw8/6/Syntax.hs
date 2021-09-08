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
    , globalFromSExpr
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
import SimpleTestsColor (test)

import Parser

-- |Variables are just strings
type Variable = String

-- |Values with respect to arithmetic
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
           | (<Variable> <Expr>+)
           | (real? <Expr>)
           | (integer? <Expr>)
           | (number? <Expr>)
           | (boolean? <Expr>)
           | (pair? <Expr>)
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
programFromSExpression :: S.Expr -> Maybe Program
programFromSExpression (S.List es) = go [] es
  where
    go defs [e] = Program (reverse defs) <$> (fromSExpression e)
    go defs (e : es) = do 
        g <- globalFromSExpr e
        go (g : defs) es

globalFromSExpr :: S.Expr -> Maybe GlobalDef
globalFromSExpr (S.List [ S.Symbol "defun", S.Symbol fname, S.List args, body]) 
    | not $ null args = 
        Defun fname <$> (mapM unSymbol args) <*> fromSExpression body
  where
    unSymbol (S.Symbol s) = Just s
    unSymbol _ = Nothing
globalFromSExpr (S.List [ S.Symbol "define", S.Symbol var, expr ]) =
    Define var <$> fromSExpression expr
globalFromSExpr _ = Nothing

-- |Parse an s-expression and convert it into a protoScheme expression.
fromSExpression :: S.Expr -> Maybe Expr
fromSExpression (S.Integer i) = Just $ Value $ Integer i
fromSExpression (S.Real r) = Just $ Value $ Float r
fromSExpression (S.Boolean b) = Just $ Value $ Boolean b
fromSExpression (S.Symbol v) = Just $ Var v
fromSExpression (S.List (S.Symbol s : es)) 
    | s `elem` keywords = fromKeyword s es
  where
    keywords = [ "+", "-", "*", "/", "<", ">", "=", "not", "and", "or"
               , "let", "if", "cond", "pair", "left", "right"
               , "real?", "integer?", "number?", "boolean?", "pair?"
               ]
fromSExpression (S.List (S.Symbol s : args@(_ : _))) = 
    Call s <$> mapM fromSExpression args
fromSExpression _ = Nothing -- error "Couldn't parse s-expression."


fromKeyword :: String -> [S.Expr] -> Maybe Expr
fromKeyword s [e] | s `elem` unOps =
    constrOfUnOp s <*> fromSExpression e
  where
    unOps :: [String]
    unOps = [ "not", "real?", "integer?", "number?", "boolean?", "pair?" ]
    constrOfUnOp :: String -> Maybe (Expr -> Expr)
    constrOfUnOp s = case s of
        "not" -> Just Not
        "real?" -> Just RealHuh
        "integer?" -> Just IntegerHuh
        "number?" -> Just NumberHuh
        "boolean?" -> Just BooleanHuh
        "pair?" -> Just PairHuh
        _ -> Nothing -- error "Invalid unOp"

fromKeyword s [e1, e2] | s `elem` binOps= do
    binOp <- constrOfBinOp s
    fromSExpression2 binOp e1 e2
  where
    binOps :: [String]
    binOps = [ "+", "-", "*", "/", "and", "or", ">", "<", "=" ]
    constrOfBinOp :: String -> Maybe (Expr -> Expr -> Expr)
    constrOfBinOp s = case s of
        "+" -> Just Add
        "-" -> Just Sub
        "*" -> Just Mul
        "/" -> Just Div
        "and" -> Just And
        "or" -> Just Or
        ">" -> Just Gt
        "<" -> Just Lt
        "=" -> Just Eq
        _ -> Nothing -- error "Invalid binOp"

fromKeyword "let" [S.List [S.Symbol v, e1], e2] =
    fromSExpression2 (Let v) e1 e2
fromKeyword "if" [e1, e2, e3] =
    fromSExpression3 If e1 e2 e3
fromKeyword "cond" es 
    | not (null es) && isElse (last es) = do
        clauses <- mapM toClause $ init es
        els <- elseClause $ last es
        return $ Cond clauses (Just els)
    | otherwise = do
        clauses <- mapM toClause es
        return $ Cond clauses Nothing
  where
    toClause :: S.Expr -> Maybe Clause
    toClause (S.List [e1, e2]) = do
        e1' <- fromSExpression e1
        e2' <- fromSExpression e2
        return (e1', e2')
    toClause _ = Nothing -- error "Invalid clause"

    isElse :: S.Expr -> Bool
    isElse (S.List [S.Symbol "else", _]) = True
    isElse _ = False

    elseClause :: S.Expr -> Maybe Expr
    elseClause (S.List [S.Symbol "else", e]) = fromSExpression e
    elseClause _ = Nothing -- error "Invalid else clause"

fromKeyword "pair" [e1, e2] = fromSExpression2 Pair e1 e2
fromKeyword "left" [e] = PLeft <$> fromSExpression e
fromKeyword "right" [e] = PRight <$> fromSExpression e
fromKeyword _ _ = Nothing


-- |Helper: convert two s-expressions and apply a binary constructor.
fromSExpression2 :: (Expr -> Expr -> Expr) -> S.Expr -> S.Expr -> Maybe Expr
fromSExpression2 c e1 e2 = c <$> (fromSExpression e1) <*> (fromSExpression e2)

-- |Helper: convert three s-expressions and apply a ternary constructor.
fromSExpression3 :: (Expr -> Expr -> Expr -> Expr) -> S.Expr -> S.Expr -> S.Expr -> Maybe Expr
fromSExpression3 c e1 e2 e3 = 
    c <$> fromSExpression e1 <*> fromSExpression e2 <*> fromSExpression e3

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
           , (S.List [S.Symbol v, (toSExpression e1)])
           , (toSExpression e2)]
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

-- |Helper: convert a symbol application to an s-expression.
toSymbolApp :: String -> [Expr] -> S.Expr
toSymbolApp s es = S.List (S.Symbol s : map toSExpression es)

-- |Convert an evaluation result into s-expression
valueToSExpression :: Value -> S.Expr
valueToSExpression (Integer i) = S.Integer i
valueToSExpression (Float f) = S.Real f
valueToSExpression (Boolean b) = S.Boolean b
valueToSExpression (PairV v1 v2) = 
    S.Dotted (valueToSExpression v1) (valueToSExpression v2)

test_fromSExpression = do
    test "fromSExpression 42" 
        (fromSExpression $ S.Integer 42) 
        (Just $ Value (Integer 42))
    test "fromSExpression y" (fromSExpression $ S.Symbol "y") (Just $ Var "y")
    test "fromExpression (let (x 42) (+ x x))" 
        (fromSExpression $ S.List 
            [ S.Symbol "let"
            , S.List [S.Symbol "x", S.Integer 42]
            , S.List [S.Symbol "+", S.Symbol "x", S.Symbol "x"
            ]])
        (Just $ Let "x" (integer 42) (Add (Var "x") (Var "x")))
    test "fromSExpression (- 12 32)"
        (fromSExpression $ S.List [ S.Symbol "-", S.Integer 12, S.Integer 32 ])
        (Just $ Sub (integer 12) (integer 32))
    test "fromSExpression (* 12 (/ 32 0))"
        (fromSExpression $ S.List 
            [ S.Symbol "*"
            , S.Integer 12
            , S.List [S.Symbol "/", S.Integer 32, S.Integer 0]])
        (Just $ Mul (integer 12) (Div (integer 32) (integer 0)))
    test "fromSExpression true" 
        (fromSExpression $ S.Boolean True) 
        (Just $ Value $ Boolean True)
    test "fromExpression (if (< 0 1) 42 3.14)" 
        (fromSExpression $ S.List 
            [ S.Symbol "if"
            , S.List [S.Symbol "<", S.Integer 0, S.Integer 1]
            , S.Integer 42
            , S.Real 3.14
            ])
        (Just $ If (Lt (integer 0) (integer 1)) (integer 42) (float 3.14))
    test "fromSExpression (and #t #f)" 
        (fromSExpression $ S.List 
            [ S.Symbol "and"
            , S.Boolean True
            , S.Boolean False
            ])
        (Just $ And (bool True) (bool False))
    test "fromSExpression (or (> 0 1) (< 0 1))" 
        (fromSExpression $ S.List 
            [ S.Symbol "or"
            , S.List [S.Symbol ">", S.Integer 0, S.Integer 1]
            , S.List [S.Symbol "<", S.Integer 0, S.Integer 1]
            ])
        (Just $ Or (Gt (integer 0) (integer 1)) (Lt (integer 0) (integer 1)))
    test "fromSExpression (not (= 42 #t))" 
        (fromSExpression $ S.List 
            [ S.Symbol "not"
            , S.List [S.Symbol "=", S.Integer 42, S.Boolean True ]
            ])
        (Just $ Not (Eq (integer 42) (bool True)))
    test "fromSExpression (cond )" 
        (fromSExpression $ S.List [S.Symbol "cond"]) 
        (Just $ Cond [] Nothing)
    test "fromSExpression (cond (else 42))" 
       (fromSExpression $ S.List [ S.Symbol "cond", S.List [S.Symbol "else", S.Integer 42 ]]) 
       (Just $ Cond [] (Just $ integer 42))
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
        (Just $ Cond 
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
        (Just $ Cond [(Eq (Var "x") (integer 4), integer 42)] (Just $ bool False))
    test "fromSExpression (pair 1 #f)"
        (fromSExpression $ 
            S.List [ S.Symbol "pair", S.Integer 1, S.Boolean False ])
        (Just $ Pair (integer 1) (bool False))
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
        (Just $ Pair (If (Not $ bool True) (integer 1) (integer 42))
              (Pair (integer 2) (integer 3)))
    test "fromSExpression (left (pair 1 2))"
        (fromSExpression $ 
            S.List [ S.Symbol "left"
                   , S.List [ S.Symbol "pair"
                            , S.Integer 1
                            , S.Integer 2
                            ]
                   ])
        (Just $ PLeft (Pair (integer 1) (integer 2)))
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
        (Just $ PRight (If (integer 42) 
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
        (Just $ Call "foo" [integer 1, Var "b", integer 3, Var "d"])


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

allTests = do
    test_fromSExpression
    test_toSExpression
