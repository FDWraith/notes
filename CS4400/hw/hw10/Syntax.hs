{-# LANGUAGE RankNTypes #-}
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
    ( Env
    , fromSExpression
    , programFromSExpression
    , programFromSExpressions
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
    , Op (..)
    , Signature (..)
    , Clause
    , Syntax.allTests
    ) where

import qualified SExpression as S
import SimpleTestsColor (test, testSection)

import Maps

import Result

import Parser

import Types hiding (fromSExpression, toSExpression)

import qualified Types as T

import Prelude hiding (fail)
import Control.Monad.Fail

-- |Variables are just strings
type Variable = String


-- |Built-in opertions with a name and arity
data Op = Op String Int ([Value] -> Result Value) 

instance Show Op where
    show (Op name arity _) = "Op " ++ name ++ " " ++ show arity

instance Eq Op where
    _ == _ = False

-- |Values
data Value = Integer Integer
           | Float Double
           | Boolean Bool
           | PairV Value Value
           | List [Value]
           | Closure [Variable] Expr Env
           | PrimApp Op [Value]
           deriving (Eq, Show)

-- |Environment for variables
type Env = Map Variable Value

{-

  <Signature> ::= ( <Variable> : <Type> )

  <Program> ::= (<GlobalDef>* <Expr>)

  <GlobalDef> ::= (defun <Variable> (<Variable>+) <Expr>)
                | (define <Variable> <Expr>)

  <Expr> ::= <Value>
           | <Variable>
           | (lambda (<Signature>*) <Expr>)
           | (let (<Variable> <Expr>) <Expr>)
           | (if <Expr> <Expr> <Expr>)
           | (and <Expr> <Expr>)
           | (or <Expr> <Expr>)
           | (cond (<Expr> <Expr>)*)
           | (cond (<Expr> <Expr>)* (else <Expr>))
           | (<Expr>+)
           | (cons <Expr> <Expr>)
           | nil
-}

-- |A _program_ is global definitions + expression
data Program = Program [GlobalDef] Expr
             deriving (Eq, Show)

-- |Global definitions
data GlobalDef = Define Signature Expr
               deriving (Eq, Show)

-- |Signatures for Types
data Signature = TySig Variable Type
           deriving (Eq, Show)

-- |protoScheme expressions
data Expr = Value Value
          | Var Variable
          | Lam [Variable] Expr
          | Let Variable Expr Expr
          | If Expr Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Cond [Clause] (Maybe Expr)
          | Call Expr [Expr]
          | Nil
          | Cons Expr Expr
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

-- |Parse a list of s-expressions as a program
programFromSExpressions :: MonadFail m => [S.Expr] -> m Program
programFromSExpressions es = programFromSExpression $ S.List es

-- |Parse a program s-expression
programFromSExpression :: MonadFail m => S.Expr -> m Program
programFromSExpression (S.List es) = go [] es
  where
    go defs [e] = Program (reverse defs) <$> fromSExpression e
    go defs (s : e : es) = do 
        g <- globalFromSExpr s e
        go (g : defs) es

globalFromSExpr :: MonadFail m => S.Expr -> S.Expr -> m GlobalDef
globalFromSExpr sig (S.List [ S.Symbol "defun", S.Symbol fname, S.List args, body]) 
    | not $ null args = do
        (TySig _ fType) <- fromSigSExpression sig
        argNames <- (mapM unSymbol args)
        -- argTypes <- align fType argNames
        body' <- fromSExpression body
        return $ Define (TySig fname fType) (Lam argNames body')
  where
    unSymbol :: MonadFail m => S.Expr -> m Variable
    unSymbol (S.Symbol s) = return $ s
    unSymbol _ = fail $ fname ++ ": expected a symbol in argument position"

    align :: MonadFail m => Type -> [Variable] -> m [Signature]
    align (TyArrow [t]) [] = return []
    align (TyArrow (t1 : ts)) (v1 : vs) = do
      rest <- align (TyArrow ts) vs
      return ( (TySig v1 t1) : rest )
    align _ _ = fail $ fname ++ ": expected signature to match function body"    
globalFromSExpr sig (S.List [ S.Symbol "define", S.Symbol var, expr ]) = do
  sig' <- fromSigSExpression sig
  e <- fromSExpression expr
  return $ Define sig' e
globalFromSExpr _ _ = fail "Unknown s-expression"

-- |Parse an s-expression and convert it into a protoScheme expression.
fromSExpression :: MonadFail m => S.Expr -> m Expr
fromSExpression (S.Integer i) = return $ Value $ Integer i
fromSExpression (S.Real r) = return $ Value $ Float r
fromSExpression (S.Boolean b) = return $ Value $ Boolean b
fromSExpression (S.Symbol "nil") = return $ Nil
fromSExpression (S.Symbol v) = return $ Var v
fromSExpression (S.List (S.Symbol s : es)) 
    | s `elem` keywords = fromKeyword s es
  where
    keywords = [ "and", "or", "lambda", "lam", "λ", "let", "if", "cond", "cons" ]
fromSExpression (S.List (ae : args@(_ : _))) = 
    Call <$> fromSExpression ae <*> mapM fromSExpression args
fromSExpression _ = fail "Couldn't parse s-expression"


-- |Parse a keyword with arguments
fromKeyword :: MonadFail m => String -> [S.Expr] -> m Expr
fromKeyword s [e1, e2] | s `elem` binOps= do
    binOp <- constrOfBinOp s
    fromSExpression2 binOp e1 e2
  where
    binOps :: [String]
    binOps = [ "and", "or", "cons"]
    constrOfBinOp :: MonadFail m => String -> m (Expr -> Expr -> Expr)
    constrOfBinOp s = case s of
        "and" -> return And
        "or" -> return Or
        "cons" -> return Cons
        _ -> fail "Invalid binOp"
fromKeyword "let" [S.List [S.Symbol v, e1], e2] =
    fromSExpression2 (Let v) e1 e2
fromKeyword s [S.List vars, e] | s `elem` ["lambda", "lam", "λ"] =
    Lam (map unSymbol vars) <$> (fromSExpression e)
    where
      unSymbol (S.Symbol s) = s
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
    toClause :: MonadFail m => S.Expr -> m Clause
    toClause (S.List [e1, e2]) = do
        e1' <- fromSExpression e1
        e2' <- fromSExpression e2
        return (e1', e2')
    toClause _ = fail "Invalid clause for cond"

    isElse :: S.Expr -> Bool
    isElse (S.List [S.Symbol "else", _]) = True
    isElse _ = False

    elseClause :: MonadFail m => S.Expr -> m Expr
    elseClause (S.List [S.Symbol "else", e]) = fromSExpression e
    elseClause _ = fail "Invalid else clause"
fromKeyword _ _ = fail "Unexpected keyword"

-- |Helper: convert a signature expression into a Signature
fromSigSExpression :: MonadFail m => S.Expr -> m Signature
fromSigSExpression (S.List [ S.Symbol name, S.Symbol ":", typeSExpr ]) = do
      ty <- case T.fromSExpression typeSExpr of
              Success s -> return s
              Failure f -> fail f
      return $ TySig name ty
fromSigSExpression e = fail $ "Expected a signature, but got " ++ show(e)

-- |Helper: convert two s-expressions and apply a binary constructor.
fromSExpression2 :: MonadFail m => (Expr -> Expr -> Expr) -> S.Expr -> S.Expr -> m Expr
fromSExpression2 c e1 e2 = c <$> fromSExpression e1 <*> fromSExpression e2

-- |Helper: convert three s-expressions and apply a ternary constructor.
fromSExpression3 :: MonadFail m => (Expr -> Expr -> Expr -> Expr) -> S.Expr -> S.Expr -> S.Expr -> m Expr
fromSExpression3 c e1 e2 e3 = 
    c <$> fromSExpression e1 <*> fromSExpression e2 <*> fromSExpression e3

-- |Convert a protoScheme expression into its s-expression representation
toSExpression :: Expr -> S.Expr
toSExpression (Value v) = valueToSExpression v
toSExpression (Var v) = S.Symbol v
toSExpression (Let v e1 e2) =
    S.List [ S.Symbol "let"
           , S.List [ S.Symbol v, toSExpression e1 ]
           , toSExpression e2
           ]
toSExpression (If e1 e2 e3) = toSymbolApp "if" [e1, e2, e3]
toSExpression (And e1 e2) = toSymbolApp "and" [e1, e2]
toSExpression (Or e1 e2) = toSymbolApp "or" [e1, e2]
toSExpression (Cond clauses elseExpr) =
    S.List $ S.Symbol "cond" : map fromClause clauses ++ elseClause elseExpr
  where 
    elseClause (Just e) = [ S.List [S.Symbol "else", toSExpression e] ]
    elseClause Nothing = []

    fromClause (e1, e2) = S.List [toSExpression e1, toSExpression e2]
toSExpression (Call e es) = S.List $ map toSExpression $ e : es
toSExpression (Lam vars body) = S.List [ S.Symbol "lambda", S.List (map asSym vars), toSExpression body ]
  where
    -- asSig (TySig s ty) = S.List [ S.Symbol s, S.Symbol ":", T.toSExpression ty]
    asSym s = S.Symbol s
toSExpression Nil = S.Symbol "nil"
toSExpression (Cons e1 e2) = toSymbolApp "cons" [e1, e2]

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
valueToSExpression (List vs) = 
    S.List $ map valueToSExpression vs
valueToSExpression (Closure xs _ _) =
    S.List [ S.Symbol "lambda", S.List $ map S.Symbol xs, S.Symbol "..." ]


-- Tests --

allTests = do
    testSection "fromSExpression"
    test_fromSExpression
    testSection "toSExpression"
    test_toSExpression

test_fromSExpression = do
    test "fromSExpression 42" 
        (fromSExpression $ S.Integer 42) 
        (Success $ Value (Integer 42))
    test "fromSExpression y" (fromSExpression $ S.Symbol "y") (Just $ Var "y")
    test "fromExpression (let (x 42) (+ x x))" 
        (fromSExpression $ S.List 
            [ S.Symbol "let"
            , S.List [S.Symbol "x", S.Integer 42]
            , S.List [S.Symbol "+", S.Symbol "x", S.Symbol "x"
            ]])
        (Success $ Let "x" (integer 42) (Call (Var "+") [Var "x", Var "x"]))
    test "fromSExpression (- 12 32)"
        (fromSExpression $ S.List [ S.Symbol "-", S.Integer 12, S.Integer 32 ])
        (Success $ Call (Var "-") [ integer 12, integer 32 ])
    test "fromSExpression (* 12 (/ 32 0))"
        (fromSExpression $ S.List 
            [ S.Symbol "*"
            , S.Integer 12
            , S.List [S.Symbol "/", S.Integer 32, S.Integer 0]])
        (Success $ 
            Call (Var "*") 
                [ integer 12
                , Call (Var "/") 
                      [ integer 32
                      , integer 0
                      ]
                ])
    test "fromSExpression true" 
        (fromSExpression $ S.Boolean True) 
        (Success $ Value $ Boolean True)
    test "fromExpression (if (< 0 1) 42 3.14)" 
        (fromSExpression $ S.List 
            [ S.Symbol "if"
            , S.List [ S.Symbol "<", S.Integer 0, S.Integer 1 ]
            , S.Integer 42
            , S.Real 3.14
            ])
        (Success $ 
            If (Call (Var "<") [ integer 0, integer 1 ]) 
               (integer 42) 
               (float 3.14))
    test "fromSExpression (and #t #f)" 
        (fromSExpression $ S.List 
            [ S.Symbol "and"
            , S.Boolean True
            , S.Boolean False
            ])
        (Success $ And (bool True) (bool False))
    test "fromSExpression (or (> 0 1) (< 0 1))" 
        (fromSExpression $ S.List 
            [ S.Symbol "or"
            , S.List [S.Symbol ">", S.Integer 0, S.Integer 1]
            , S.List [S.Symbol "<", S.Integer 0, S.Integer 1]
            ])
        (Success $
          Or (Call (Var ">") [ integer 0, integer 1 ])
             (Call (Var "<") [ integer 0, integer 1 ]))
    test "fromSExpression (not (= 42 #t))" 
        (fromSExpression $ S.List 
            [ S.Symbol "not"
            , S.List [S.Symbol "=", S.Integer 42, S.Boolean True ]
            ])
        (Success $
          Call (Var "not") [Call (Var "=") [integer 42, bool True] ])
    test "fromSExpression (cond )" 
        (fromSExpression $ S.List [S.Symbol "cond"]) 
        (Success $ Cond [] Nothing)
    test "fromSExpression (cond (else 42))" 
       (fromSExpression $ S.List [ S.Symbol "cond", S.List [S.Symbol "else", S.Integer 42 ]]) 
       (Success $ Cond [] (return $ integer 42))
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
        (Success $ Cond 
            [ ( (Call (Var "=") [Var "x", integer 4]), integer 42 )
            , ( (Call (Var ">") [integer 3, integer 0]), integer 4 )
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
        (Success $ Cond [(Call (Var "=") [(Var "x"), (integer 4)], integer 42)] (return $ bool False))
    test "fromSExpression (pair 1 #f)"
        (fromSExpression $ 
            S.List [ S.Symbol "pair", S.Integer 1, S.Boolean False ])
        (Success $ Call (Var "pair") [(integer 1), (bool False)])
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
        (Success $ Call (Var "pair")
          [(If (Call (Var "not") [bool True]) (integer 1) (integer 42)),
           (Call (Var "pair") [(integer 2), (integer 3)])
          ])
    test "fromSExpression (left (pair 1 2))"
        (fromSExpression $ 
            S.List [ S.Symbol "left"
                   , S.List [ S.Symbol "pair"
                            , S.Integer 1
                            , S.Integer 2
                            ]
                   ])
        (Success $ Call (Var "left") [(Call (Var "pair") [(integer 1), (integer 2)])])
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
        (Success $ Call (Var "right")
          [(If (integer 42) 
             (Call (Var "pair") [(bool True), (bool False)]) 
             (Call (Var "+") [(integer 12), (integer 13)]))
          ])
    test "fromSExpression (foo 1 b 3 d)"
        (fromSExpression $
            S.List [ S.Symbol "foo"
                   , S.Integer 1
                   , S.Symbol "b"
                   , S.Integer 3
                   , S.Symbol "d"
                   ])
        (Success $ Call (Var "foo") [integer 1, Var "b", integer 3, Var "d"])
    test "fromSExpression nil"
        (fromSExpression $ S.Symbol "nil")
        (Success $ Nil)
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
            (Call (Var "+")
             [(Call (Var "-") [(integer 12), (integer 1)]),
              (Call (Var "*") [(integer 10),
               (Call (Var "/") [(integer 12), (integer 4)])])
             ]))
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
            And (Or (Call (Var "<") [(integer 1), (integer 2)])
                    (Call (Var ">") [(Var "x"), (Var "y")]))
                (Call (Var "not") [(Call (Var "=") [(Var "x"), (integer 2)])]))
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
            Cond [ ( Call (Var "=") [Var "x", Var "y"]
                   , Call (Var "+") [integer 1, integer 2]
                   )
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
            Cond [ (Call (Var "=") [(Var "x"), (Var "y")], Call (Var "+") [(integer 1), (integer 2)])
                 , (bool False, integer 42)
                 ] 
                 (return $ float 3.14))
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
            Call (Var "pair") 
              [ Call (Var "left") [Call (Var "pair") [integer 1, integer 2] ]
              , Call (Var "right") [Call (Var "pair") [integer 3, integer 4]]])
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
    test "toSExpression (lambda (x) x)"
      (toSExpression $ Lam ["x"] (Var "x"))
      (S.List [ S.Symbol "lambda"
              , S.List [ S.Symbol "x" ] 
              , S.Symbol "x"])
