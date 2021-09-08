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
    , programFromSExpressions
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
    , Clause
    , Syntax.allTests
    ) where

import qualified SExpression as S
import qualified Types as T
import Maps
import Result

import Control.Monad (when)

import SimpleTestsColor (test, testSection)

-- |Variables are just strings
type Variable = String


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

type Env = Map Variable Value

{-

  <Program> ::= (<GlobalDef>* <Expr>)

  <GlobalDef> ::= (defun <Variable> (<Variable>+) <Expr>)
                | (define <Variable> <Expr>)

  <Expr> ::= <Value>
           | <Variable>
           | (lambda (<Variable>*) <Expr>)
           | (let (<Variable> <Expr>) <Expr>)
           | (if <Expr> <Expr> <Expr>)
           | (and <Expr> <Expr>)
           | (or <Expr> <Expr>)
           | (cond (<Expr> <Expr>)*)
           | (cond (<Expr> <Expr>)* (else <Expr>))
           | (pair <Expr> <Expr>)
           | (left <Expr>)
           | (right <Expr>)
           | (<Expr>+)
-}

-- |A _program_ is global definitions + expression
data Program = Program [GlobalDef] Expr
             deriving (Eq, Show)

-- |Global definitions
data GlobalDef = Define Variable T.Type Expr
               deriving (Eq, Show)

-- |protoScheme expressions
data Expr = Value Value
          | Var Variable
          | Lam [(Variable, T.Type)] Expr
          | Let Variable Expr Expr
          | If Expr Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Cond [Clause] (Maybe Expr)
          | Call Expr [Expr]
          | Pair Expr Expr
          | PLeft Expr
          | PRight Expr
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
programFromSExpressions :: [S.Expr] -> Result Program
programFromSExpressions [] = 
    fail "programFromSExpressions: expected at least 1 s-expression"
programFromSExpressions es = go [] es
  where
    go _ [] = 
        fail "programFromSExpressions: expected an odd number of s-expressions"
    go defs [e] = Program (reverse defs) <$> fromSExpression e
    go defs (sig : def : e) = do
        (x, ty) <- sigFromSExpression sig
        global@(Define x' _ _) <- defFromSExpression ty def
        when (x /= x') $ 
          fail $ "Signature and definition do no match: sig is for " ++
              x ++ ", def for " ++ x'
        go (global : defs) e

    sigFromSExpression (S.List [ S.Symbol x, S.Symbol ":", se ]) = do
        ty <- T.fromSExpression se
        return (x, ty)
    sigFromSExpression _ = fail "Expected a signature"

defFromSExpression :: T.Type -> S.Expr -> Result GlobalDef
defFromSExpression ty (S.List [ S.Symbol "defun", S.Symbol fname, S.List args, body ]) = do
    args' <- mapM unSymbol args
    tys <- argTypes ty
    Define fname ty <$> (Lam (zip args' tys) <$> fromSExpression body)
  where
    argTypes (T.TyArrow ts) = return $ init ts
    argTypes _ = fail $ "Expected a function type in signature for " ++ fname
    unSymbol (S.Symbol s) = return s
    unSymbol _ = fail $ fname ++ ": expected a symbol in argument position"

defFromSExpression ty (S.List [ S.Symbol "define", S.Symbol var, expr ]) =
    Define var ty <$> fromSExpression expr

defFromSExpression _ _ = fail "Unknown definition"


-- |Parse an s-expression and convert it into a protoScheme expression.
fromSExpression :: S.Expr -> Result Expr
fromSExpression (S.Integer i) = return $ Value $ Integer i
fromSExpression (S.Real r) = return $ Value $ Float r
fromSExpression (S.Boolean b) = return $ Value $ Boolean b
fromSExpression (S.Symbol v) = return $ Var v
fromSExpression (S.List (S.Symbol s : es)) 
    | s `elem` keywords = fromKeyword s es
  where
    keywords = [ "and", "or", "lambda", "lam", "λ", "let", "if", "cond" 
               , "pair", "left", "right" ]
fromSExpression (S.List (ae : args@(_ : _))) = 
    Call <$> fromSExpression ae <*> mapM fromSExpression args
fromSExpression _ = fail "Couldn't parse s-expression"


fromKeyword :: String -> [S.Expr] -> Result Expr
fromKeyword kw [e1, e2] | kw `elem` keys binOps = do
    binOp <- constrOfBinOp
    fromSExpression2 binOp e1 e2
  where
    binOps :: Map String (Expr -> Expr -> Expr)
    binOps = fromList 
        [ ("and", And)
        , ("or", Or)
        , ("pair", Pair) 
        ]
    constrOfBinOp = fromMaybe' "Invalid binOp" $ get binOps kw

fromKeyword "let" [S.List [S.Symbol v, e1], e2] =
    fromSExpression2 (Let v) e1 e2
fromKeyword kw [S.List vars, e] | kw `elem` ["lambda", "lam", "λ"] =
    Lam <$> mapM arg vars <*> fromSExpression e
  where
    arg (S.List [S.Symbol s, S.Symbol ":", se]) = do 
        ty <- T.fromSExpression se 
        return (s, ty)
    arg _ = fail "lambda: expected a [ variable : type ] in argument position"
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
    toClause :: S.Expr -> Result Clause
    toClause (S.List [e1, e2]) = do
        e1' <- fromSExpression e1
        e2' <- fromSExpression e2
        return (e1', e2')
    toClause _ = fail "Invalid clause for cond"

    isElse :: S.Expr -> Bool
    isElse (S.List [S.Symbol "else", _]) = True
    isElse _ = False

    elseClause :: S.Expr -> Result Expr
    elseClause (S.List [S.Symbol "else", e]) = fromSExpression e
    elseClause _ = fail "Invalid else clause"
fromKeyword "left" [e] = PLeft <$> fromSExpression e
fromKeyword "right" [e] = PRight <$> fromSExpression e
fromKeyword _ _ = fail "Unexpected keyword"


-- |Helper: convert two s-expressions and apply a binary constructor.
fromSExpression2 :: (Expr -> Expr -> Expr) -> S.Expr -> S.Expr -> Result Expr
fromSExpression2 c e1 e2 = c <$> fromSExpression e1 <*> fromSExpression e2

-- |Helper: convert three s-expressions and apply a ternary constructor.
fromSExpression3 
    :: (Expr -> Expr -> Expr -> Expr) 
    -> S.Expr 
    -> S.Expr 
    -> S.Expr 
    -> Result Expr
fromSExpression3 c e1 e2 e3 = 
    c <$> fromSExpression e1 <*> fromSExpression e2 <*> fromSExpression e3

-- |Convert a protoScheme expression into its s-expression representation
toSExpression :: Expr -> S.Expr
toSExpression (Value v) = valueToSExpression v
toSExpression (Var v) = S.Symbol v
toSExpression (Lam args e) = 
    S.List [ S.Symbol "lambda"
           , S.List $ fromSignature <$> args
           , toSExpression e 
           ]
  where
    fromSignature (x, ty) = 
        S.List [ S.Symbol x, S.Symbol ":", T.toSExpression ty ]
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
toSExpression (Pair e1 e2) = toSymbolApp "pair" [e1, e2]
toSExpression (PLeft e) = toSymbolApp "left" [e]
toSExpression (PRight e) = toSymbolApp "right" [e]

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
valueToSExpression (PrimApp (Op name _ _) _) =
    S.Symbol $ "<operation " ++ name ++ ">"


-- Tests --

allTests :: IO ()
allTests = do
    testSection "fromSExpression"
    test_fromSExpression
    test_toSExpression

test_fromSExpression :: IO ()
test_fromSExpression = do
    test "fromSExpression 42" 
        (fromSExpression $ S.Integer 42) 
        (Success $ Value (Integer 42))
    test "fromSExpression y" (fromSExpression $ S.Symbol "y") (Success $ Var "y")
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
        (return $ Or (Call (Var ">") [integer 0, integer 1]) 
            (Call (Var "<")  [integer 0, integer 1]))
    test "fromSExpression (not (= 42 #t))" 
        (fromSExpression $ S.List 
            [ S.Symbol "not"
            , S.List [S.Symbol "=", S.Integer 42, S.Boolean True ]
            ])
        (return $ Call (Var "not") [Call (Var "=") [integer 42, bool True]])
    test "fromSExpression (cond )" 
        (fromSExpression $ S.List [S.Symbol "cond"]) 
        (return $ Cond [] Nothing)
    test "fromSExpression (cond (else 42))" 
       (fromSExpression $ 
            S.List [ S.Symbol "cond", S.List [S.Symbol "else", S.Integer 42 ]]) 
       (return $ Cond [] (return $ integer 42))
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
        (return $ Cond 
            [ (Call (Var "=") [Var "x", integer 4], integer 42)
            , (Call (Var ">") [integer 3, integer 0], integer 4)
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
        (return $ Cond [(Call (Var "=") [Var "x", integer 4], integer 42)] 
              (Just $ bool False))
    test "fromSExpression (pair 1 #f)"
        (fromSExpression $ 
            S.List [ S.Symbol "pair", S.Integer 1, S.Boolean False ])
        (return $ Pair (integer 1) (bool False))
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
        (return $ Pair (If (Call (Var "not") [bool True]) (integer 1) (integer 42))
              (Pair (integer 2) (integer 3)))
    test "fromSExpression (left (pair 1 2))"
        (fromSExpression $ 
            S.List [ S.Symbol "left"
                   , S.List [ S.Symbol "pair"
                            , S.Integer 1
                            , S.Integer 2
                            ]
                   ])
        (return $ PLeft (Pair (integer 1) (integer 2)))
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
        (return $ PRight (If (integer 42) 
                    (Pair (bool True) (bool False)) 
                    (Call (Var "+") [integer 12, integer 13])))
    test "fromSExpression (foo 1 b 3 d)"
        (fromSExpression $
            S.List [ S.Symbol "foo"
                   , S.Integer 1
                   , S.Symbol "b"
                   , S.Integer 3
                   , S.Symbol "d"
                   ])
        (return $ Call (Var "foo") [integer 1, Var "b", integer 3, Var "d"])


test_toSExpression :: IO ()
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
            And (Or (Call (Var "<") [integer 1, integer 2])
                    (Call (Var ">") [Var "x", Var "y"]))
                (Call (Var "not") [Call (Var "=") [Var "x", integer 2]]))
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
            Cond [ ( Call (Var "=") [ Var "x", Var "y" ]
                   , Call (Var "+") [ integer 1, integer 2 ]
                   )
                 , ( bool False
                   , integer 42
                   )
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

                          
