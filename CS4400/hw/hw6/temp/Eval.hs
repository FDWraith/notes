{-|
Module      : Eval
Description : Semantics of protoScheme
Copyright   : (c) Ferd, 2020
                  Jack, 2020
Maintainer  : f.vesely@northeastern
              gelinas.j@northeastern.edu 

This module provides the evaluator of the protoScheme language.
-}
module Eval
    ( subst
    , eval
    , runSExpression
    , Eval.allTests
    ) where

import Syntax
import Maps
import qualified SExpression as S
import SimpleTestsColor (test, testSection)

-- |To represent functions with arbitrarily many arguments
data Function = Function [Variable] Expr

-- |Global function environment
type FunEnv = Map Variable Function

-- |Global value environment
type GlobalEnv = Map Variable Value

-- | Evaluate a program
evalProgram :: Program -> Maybe Value
evalProgram (Program globals expr) = do
    (fenv, genv) <- evalGlobals ([], []) globals
    eval fenv genv expr
  where
    -- |Evaluate all globals in a program, using the current global environment
    evalGlobals 
        :: (FunEnv, GlobalEnv) -> [GlobalDef] -> Maybe (FunEnv, GlobalEnv)
    evalGlobals envs (def : globals) = do
        envs' <- evalGlobal envs def
        evalGlobals envs' globals
    evalGlobals envs [] = return envs

    -- |Evaluate a global definition
    evalGlobal :: (FunEnv, GlobalEnv) -> GlobalDef -> Maybe (FunEnv, GlobalEnv)
    evalGlobal (_, genv) (Define x _) 
        | x `elem` keys genv = Nothing -- prohibit redefining variables
    evalGlobal (fenv, genv) (Define x expr) = do
        v <- eval fenv genv expr
        return (fenv, set genv x v)
    evalGlobal (fenv, _) (Defun f _ _) 
        | f `elem` keys fenv = Nothing -- prohibit redefining functions
    evalGlobal (fenv, genv) (Defun f args body) = 
        return (set fenv f (Function args body), genv)
        
-- |Evaluates the given expression to a value.
eval :: FunEnv -> GlobalEnv -> Expr -> Maybe Value
eval _ _ (Value v) = Just v
eval _ genv (Var x) = get genv x

-- arithmetic
eval fenv genv (Add e1 e2) = 
    liftArith (+) (+) (eval fenv genv e1) (eval fenv genv e2)
eval fenv genv (Sub e1 e2) = 
    liftArith (-) (-) (eval fenv genv e1) (eval fenv genv e2)
eval fenv genv (Mul e1 e2) = 
    liftArith (*) (*) (eval fenv genv e1) (eval fenv genv e2)
eval fenv genv (Div e1 e2) = 
    divide (eval fenv genv e1) (eval fenv genv e2)

-- let bindings
eval fenv genv (Let x e1 e2) =
    case eval fenv genv e1 of
         Just v -> eval fenv genv (subst x v e2)
         Nothing -> Nothing

-- boolean operations & conditionals
eval fenv genv (If e1 e2 e3) =
    case eval fenv genv e1 of
         Just (Boolean b) | b -> eval fenv genv e2
                          | not b -> eval fenv genv e3
         _ -> Nothing
eval fenv genv (And e1 e2) =
    case eval fenv genv e1 of
         Just (Boolean b) | b -> eval fenv genv e2
                          | not b -> Just $ Boolean False
         _ -> Nothing
eval fenv genv (Or e1 e2) =
    case eval fenv genv e1 of
         Just (Boolean b) | b -> Just $ Boolean True
                          | not b -> eval fenv genv e2
         _ -> Nothing
eval fenv genv (Not e) =
    case eval fenv genv e of
         Just (Boolean b) -> Just $ Boolean (not b)
         _ -> Nothing
eval fenv genv (Lt e1 e2) = 
    liftM2 (liftOrd (<) (<)) 
        (eval fenv genv e1) 
        (eval fenv genv e2)
eval fenv genv (Gt e1 e2) = 
    liftM2 (liftOrd (>) (>)) 
        (eval fenv genv e1) 
        (eval fenv genv e2)
eval fenv genv (Eq e1 e2) = 
    liftM2 typEq (eval fenv genv e1) (eval fenv genv e2)
eval fenv genv (Cond clauses els) =
    evalClauses clauses els
  where
    evalClauses [] (Just e) = eval fenv genv e
    evalClauses [] Nothing = Nothing
    evalClauses ((e1, e2) : rest) els =
        case eval fenv genv e1 of
             Just (Boolean b) | b -> eval fenv genv e2
                              | not b -> evalClauses rest els
             _ -> Nothing

-- pairs:

{- Monadic notation - either using do:
  eval fenv genv (Pair e1 e2) = do
      v1 <- eval fenv genv e1
      v2 <- eval fenv genv e2
      return $ PairV v1 v2
  or using the >>= operator: -}
eval fenv genv (Pair e1 e2) =
    eval fenv genv e1 >>= \v1 ->
    eval fenv genv e2 >>= \v2 ->
    return $ PairV v1 v2
eval fenv genv (PLeft e) = do
    PairV v1 _ <- eval fenv genv e -- if this pattern match fails, the result is nothing, automatically
    return v1
eval fenv genv (PRight e) = do
    PairV _ v2 <- eval fenv genv e
    return v2

-- function calls:
eval fenv genv (Call f es) = do
    vs <- evalArgs es
    Function args body <- get fenv f
    eval fenv genv $ substArgs (zip args vs) body
  where
    evalArgs :: [Expr] -> Maybe [Value]
    evalArgs [] = return []
    evalArgs (e : es) = do
      v <- eval fenv genv e
      vs <- evalArgs es
      return (v : vs)
    -- alternatively just:
    -- evalArgs es = mapM (eval fenv genv) es
    
    substArgs :: [(Variable, Value)] -> Expr -> Expr
    substArgs [] body = body
    substArgs ((x, v) : args) body = substArgs args (subst x v body) 
    -- alternatively just: 
    -- substArgs args body = foldl (\e (x, v) -> subst x v e) body args

-- type predicates: 
eval fenv genv (RealHuh e) = 
    case eval fenv genv e of
         Just (Float _) -> Just $ Boolean True
         Just _ -> Just $ Boolean False
         Nothing -> Nothing
eval fenv genv (IntegerHuh e) = 
    case eval fenv genv e of
         Just (Integer _) -> Just $ Boolean True
         Just _ -> Just $ Boolean False
         Nothing -> Nothing
eval fenv genv (NumberHuh e) = 
    case eval fenv genv e of
         Just (Float _) -> Just $ Boolean True
         Just (Integer _) -> Just $ Boolean True
         Just _ -> Just $ Boolean False
         Nothing -> Nothing
eval fenv genv (BooleanHuh e) = 
    case eval fenv genv e of
         Just (Boolean _) -> Just $ Boolean True
         Just _ -> Just $ Boolean False
         Nothing -> Nothing
eval fenv genv (PairHuh e) = 
    case eval fenv genv e of
         Just (PairV _ _) -> Just $ Boolean True
         Just _ -> Just $ Boolean False
         Nothing -> Nothing


-- |Substitutes the given value for the given variable in the given expression.
subst :: Variable -> Value -> Expr -> Expr
subst _ _ v@(Value _) = v
subst x v (Var y) | x == y = Value v
                  | otherwise = Var y
subst x v (Add e1 e2) = Add (subst x v e1) (subst x v e2)
subst x v (Sub e1 e2) = Sub (subst x v e1) (subst x v e2)
subst x v (Mul e1 e2) = Mul (subst x v e1) (subst x v e2)
subst x v (Div e1 e2) = Div (subst x v e1) (subst x v e2)
subst x v (Let y e1 e2) | x == y = Let y (subst x v e1) e2
                        | otherwise = Let y (subst x v e1) (subst x v e2)
subst x v (If e1 e2 e3) = If (subst x v e1) (subst x v e2) (subst x v e3)
subst x v (And e1 e2) = subst2 x v And e1 e2
subst x v (Or e1 e2) = subst2 x v Or e1 e2
subst x v (Not e) = Not $ subst x v e
subst x v (Lt e1 e2) = subst2 x v Lt e1 e2
subst x v (Gt e1 e2) = subst2 x v Gt e1 e2
subst x v (Eq e1 e2) = subst2 x v Eq e1 e2
subst x v (Cond clauses els) =
    Cond (map substClause clauses) (substMaybe els)
  where
    substClause :: Clause -> Clause
    substClause (e1, e2) = (subst x v e1, subst x v e2)

    substMaybe :: Maybe Expr -> Maybe Expr
    substMaybe (Just e) = Just $ subst x v e
    substMaybe Nothing = Nothing
subst x v (Pair e1 e2) = subst2 x v Pair e1 e2
subst x v (PLeft e) = PLeft $ subst x v e
subst x v (PRight e) = PRight $ subst x v e
subst x v (Call f es) = Call f $ map (subst x v) es
subst x v (RealHuh e) = RealHuh $ subst x v e
subst x v (IntegerHuh e) = IntegerHuh $ subst x v e
subst x v (NumberHuh e) = NumberHuh $ subst x v e
subst x v (BooleanHuh e) = BooleanHuh $ subst x v e
subst x v (PairHuh e) = PairHuh $ subst x v e

-- |Substitution helper for 2 argument constructors.
subst2 :: Variable -> Value -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
subst2 x v ctr e1 e2 = ctr (subst x v e1) (subst x v e2)


-- |A binary operation type
type BinOp a = (a -> a -> a)


-- |Applies the appropriate arithmetic operation on the given values
evalArith :: BinOp Integer -> BinOp Double -> Value -> Value -> Maybe Value
evalArith intOp _ (Integer i1) (Integer i2) = 
    Just $ Integer $ i1 `intOp` i2
evalArith _ fltOp (Integer i1) (Float f2) = 
    Just $ Float $ fromIntegral i1 `fltOp` f2
evalArith _ fltOp (Float f1) (Integer i2) = 
    Just $ Float $ f1 `fltOp` fromIntegral i2
evalArith _ fltOp (Float f1) (Float f2) = 
    Just $ Float $ f1 `fltOp` f2
evalArith _ _ _ _ = Nothing


-- |Applies the given function to the lift maybe expressions
liftM2 :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
liftM2 fn (Just a) (Just b) = fn a b
liftM2 _ _ _ = Nothing

-- |Applies the arithmetic coersion and appliation on the lifted results of the evaluated expressions
liftArith 
    :: BinOp Integer 
    -> BinOp Double 
    -> Maybe Value 
    -> Maybe Value 
    -> Maybe Value
liftArith intOp fltOp v1 v2 = liftM2 (evalArith intOp fltOp) v1 v2

-- |Applies the arithmetic coersion and divisino on the lifted results of the evaluated expressions
divide :: Maybe Value -> Maybe Value -> Maybe Value
divide v1 v2 = liftM2 liftedDiv v1 v2
  where 
    liftedDiv :: Value -> Value -> Maybe Value
    liftedDiv v1 (Integer 0) = Nothing
    liftedDiv v1 (Float 0) = Nothing
    liftedDiv v1 v2 = evalArith div (/) v1 v2


-- |Ordering operations
liftOrd 
    :: (Integer -> Integer -> Bool) 
    -> (Double -> Double -> Bool) 
    -> Value 
    -> Value 
    -> Maybe Value
liftOrd intOp _ (Integer i1) (Integer i2) = Just (Boolean $ i1 `intOp` i2)
liftOrd _ flOp (Float f1) (Float f2) = Just (Boolean $ f1 `flOp` f2)
liftOrd _ flOp (Integer i1) (Float f2) = Just (Boolean $ fromInteger i1 `flOp` f2)
liftOrd _ flOp (Float f1) (Integer i2) = Just (Boolean $ f1 `flOp` fromInteger i2)
liftOrd _ _ _ _ = Nothing

-- |Check if the type of the given values matches
typeMatches :: Value -> Value -> Bool
typeMatches (Boolean _) (Boolean _) = True
typeMatches (Float _) (Float _) = True
typeMatches (Integer _) (Integer _) = True
typeMatches _ _ = False

-- |Typed equality: returns Nothing if the types don't match
typEq :: Value -> Value -> Maybe Value
typEq v1 v2 | typeMatches v1 v2 = Just $ Boolean $ v1 == v2
            | otherwise = Nothing

-- |Get all the names provided by a map.
keys :: Map k v -> [k]
keys m = map fst m

-- |Run the given protoScheme s-expression, returning an s-expression
-- representation of the result.
runSExpression :: S.Expr -> Maybe S.Expr
runSExpression se =
    case eval empty empty (fromSExpression se) of
         Just v -> Just (valueToSExpression v)
         Nothing -> Nothing

test_evalProgram = do
    test "((define x 10) x)"
        (evalProgram $
            Program [Define "x" (integer 10)] (Var "x"))
        (Just $ Integer 10)
    test "((define x 10) y)"
        (evalProgram $
            Program [Define "x" (integer 10)] (Var "y"))
        Nothing
    test "((define x 10) (let (x #t) x))"
        (evalProgram $
            Program [Define "x" (integer 10)] (Let "x" (bool True) (Var "x")))
        (Just $ Boolean True)
    test "((define x 10) (let (x (+ x 32)) x))"
        (evalProgram $
            Program 
              [ Define "x" (integer 10) ] 
              (Let "x" (Add (Var "x") (integer 32)) (Var "x")))
        (Just $ Integer 42)
    test "((define x (and #t #f)) (define y (if x x 42)) y)"
        (evalProgram $
            Program 
              [ Define "x" (And (bool True) (bool False)) 
              , Define "y" (If (Var "x") (Var "x") (integer 42))
              ] 
              (Var "y"))
        (Just $ Integer 42)
    test "((define x (and #t #f)) (define y (let (x 21) (* x 2))) y)"
        (evalProgram $
            Program 
              [ Define "x" (And (bool True) (bool False)) 
              , Define "y" (Let "x" (integer 21) (Mul (Var "x") (integer 2)))
              ] 
              (Var "y"))
        (Just $ Integer 42)
    test "((defun even (x) (...)) (defun odd (x) (...)) (pair (even 42) (odd 42)))"
        (evalProgram $
            Program
                [ Defun "even" ["x"] 
                    (Or (Eq (Var "x") (integer 0)) 
                        (And (Gt (Var "x") (integer 0))
                             (Call "odd" [Sub (Var "x") (integer 1)])))
                , Defun "odd" ["x"]
                    (Or (Eq (Var "x") (integer 1))
                        (And (Gt (Var "x") (integer 0))
                             (Call "even" [Sub (Var "x") (integer 1)])))
                ]
                (Pair (Call "even" [integer 42]) (Call "odd" [integer 42])))
        (Just $ PairV (Boolean True) (Boolean False))
    test "((define one 1) (defun f (x) (if (< x 2) one (* x (f (- x one))))) (f 5))"
        (evalProgram $
            Program 
              [ Define "one" (integer 1) 
              , Defun "f" ["x"] 
                    (If (Lt (Var "x") (integer 2)) 
                        (Var "one") 
                        (Mul (Var "x") (Call "f" [ Sub (Var "x") (Var "one") ])))
              ] 
              (Call "f" [integer 5]))
        (Just $ Integer 120)


test_eval = do
    test "eval (+ 21 23)" 
        (eval' (Add (integer 21) (integer 23)))
        (Just (Integer 44))
    test "eval (- 21.3 23)" 
        (eval' $ Sub (float 21.3) (integer 23))
        (Just $ Float (21.3 - 23))
    test "eval (* 3 1.2)" 
        (eval' $ Mul (integer 3) (float 1.2))
        (Just $ Float (3 * 1.2))
    test "eval (/ 5 2)"
        (eval' $ Div (integer 5) (integer 2))
        (Just $ Integer 2)
    test "eval (/ 5.0 2)"
        (eval' $ Div (float 5) (integer 2))
        (Just $ Float 2.5)
    test "eval (/ 5 2.0)"
        (eval' $ Div (integer 5) (float 2))
        (Just $ Float 2.5)
    test "eval (/ 5 0)"
        (eval' $ Div (integer 5) (integer 0))
        Nothing
    test "eval (let (x 1) x)"
        (eval' $ Let "x" (integer 1) (Var "x"))
        (Just $ Integer 1)
    test "eval (let (x 1) y)"
        (eval' $ Let "x" (integer 1) (Var "y"))
        Nothing
    test "eval (let (x 12) (+ x (let (x (+ x 1)) (+ x (let (x (+ x 1)) x)))))"
        (eval' $ Let "x" (integer 12) 
                    (Add (Var "x")
                         (Let "x" (Add (Var "x") (integer 1))
                              (Add (Var "x")
                                   (Let "x" (Add (Var "x") (integer 1))
                                        (Var "x"))))))
        (Just $ Integer (let x = 12 in x + let y = x + 1 in y + let z = y + 1 in z))
    test "eval (if (and #t (not #f)) (or (not #t) #f) 42)"
        (eval' $ If (And (bool True) (Not (bool False)))
                   (Or (Not (bool True)) (bool False))
                   (integer 42))
        (Just $ Boolean False)
    test "eval (and #f (/ 1 0))"
        (eval' $ And (bool False) (Div (integer 1) (integer 0)))
        (Just $ Boolean False)
    test "eval (and (/ 1 0) #f)"
        (eval' $ And (Div (integer 1) (integer 0)) (bool False))
        Nothing
    test "eval (or #t (/ 1 0))"
        (eval' $ Or (bool True) (Div (integer 1) (integer 0)))
        (Just $ Boolean True)
    test "eval (and (> 1 0) (< 0 1))"
        (eval' $ And (Gt (integer 1) (integer 0)) (Lt (integer 0) (integer 1)))
        (Just $ Boolean True)
    test "eval (and (> 1.1 0) (< -1 0.1))"
        (eval' $ And (Gt (float 1.1) (integer 0)) (Lt (integer $ -1) (float 0.1)))
        (Just $ Boolean True)
    test "eval (< #t 1)"
        (eval' $ Lt (bool True) (integer 1))
        Nothing
    test "eval (= #t #t)"
        (eval' $ Eq (bool True) (bool True))
        (Just $ Boolean True)
    test "eval (= #t #f)"
        (eval' $ Eq (bool True) (bool False))
        (Just $ Boolean False)
    test "eval (= 1 1)"
        (eval' $ Eq (integer 1) (integer 1))
        (Just $ Boolean True)
    test "eval (= #f 0)"
        (eval' $ Eq (bool False) (integer 0))
        Nothing
    test "eval (= 1.0 1)"
        (eval' $ Eq (float 1) (integer 1))
        Nothing
    test "eval (cond)"
        (eval' $ Cond [] Nothing)
        Nothing
    test "eval (cond (else 42))"
        (eval' $ Cond [] (Just $ integer 42))
        (Just $ Integer 42)
    test "eval (cond ((= 2 2) 1) (else 42))"
        (eval' $ Cond [(Eq (integer 2) (integer 2), integer 1)] (Just $ integer 42))
        (Just $ Integer 1)
    test "eval (cond ((= 2 2) 1) ((= 3 3) 2) (else 42))"
        (eval' $ Cond [ (Eq (integer 2) (integer 2), integer 1)
                     , (Eq (integer 3) (integer 3), integer 2)
                     ] (Just $ integer 42))
        (Just $ Integer 1)
    test "eval (cond ((= 2 3) 1) ((= 3 3) 2))"
        (eval' $ Cond [ (Eq (integer 2) (integer 3), integer 1)
                     , (Eq (integer 3) (integer 3), integer 2)
                     ] Nothing)
        (Just $ Integer 2)
    test "eval (cond ((= 2 3) 1) ((= 3 4) 2) (else 42))"
        (eval' $ Cond [ (Eq (integer 2) (integer 3), integer 1)
                     , (Eq (integer 3) (integer 4), integer 2)
                     ] (Just $ integer 42))
        (Just $ Integer 42)
    test "eval (cond ((= 2 3) 1) ((= 3 4) 2))"
        (eval' $ Cond [ (Eq (integer 2) (integer 3), integer 1)
                     , (Eq (integer 3) (integer 4), integer 2)
                     ] Nothing)
        Nothing
    test "eval (pair (pair 1 2) (pair 3 (+ 2 2)))"
        (eval' $ 
            Pair (Pair (integer 1) (integer 2)) 
                 (Pair (integer 3) (Add (integer 2) (integer 2))))
        (Just $ PairV (PairV (Integer 1) (Integer 2))
                      (PairV (Integer 3) (Integer 4)))
    test "eval (left (right (pair (pair 1 2) (pair 3 4))))"
        (eval' $ 
            PLeft (PRight (Pair (Pair (integer 1) (integer 2)) 
                          (Pair (integer 3) (integer 4)))))
        (Just $ Integer 3)
    test "eval (right (left (pair (pair 1 2) (pair 3 4))))"
        (eval' $ 
            PRight (PLeft (Pair (Pair (integer 1) (integer 2)) 
                          (Pair (integer 3) (integer 4)))))
        (Just $ Integer 2)
    test "eval (left (+ 1 2))"
        (eval' $ PLeft (Add (integer 1) (integer 2)))
        Nothing
    test "eval (right (and #t #f))"
        (eval' $ PRight (And (bool True) (bool False)))
        Nothing
    test "eval (f 10) with (f x) = (+ x y), y = -1"
        (eval 
            (set empty "f" (Function ["x"] (Add (Var "x") (Var "y"))))
            (set empty "y" (Integer $ -1))
            (Call "f" [integer 10]))
        (Just $ Integer 9)
    test "eval (f 1 2 3) with (f x y z) = (pair z (pair y x))"
        (eval 
            (set empty "f" 
                (Function ["x", "y", "z"] 
                (Pair (Var "z") (Pair (Var "y") (Var "x")))))
            empty
            (Call "f" [integer 1, integer 2, integer 3]))
        (Just $ PairV (Integer 3) (PairV (Integer 2) (Integer 1)))
    test "eval (f 10) with f = 10 fails"
        (eval empty (set empty "f" $ Integer 10) (Call "f" [integer 10]))
        Nothing
    test "eval x with (x y) = 10 fials"
        (eval (set empty "x" (Function ["y"] $ integer 10))
            empty
            (Var "x"))
        Nothing
  where
    eval' = eval empty empty

test_subst = do
    test "subst \"x\" 42 big 'un"
        (subst "x" (Integer 42)
            (Add (Sub (Div (Var "x") (Var "x")) 
                      (If (And (Var "x") (Var "y")) 
                          (Or (Var "x") (Var "y"))
                          (Not (Var "x")))) 
                 (Mul (Let "x" (Var "x") (Var "y"))
                    (Lt (Gt (Var "x") (Var "y")) 
                        (Let "x" (integer 12) (Eq (Var "y") (Var "x")))))))
        (Add (Sub (Div (integer 42) (integer 42)) 
                  (If (And (integer 42) (Var "y")) 
                      (Or (integer 42) (Var "y")) 
                      (Not (integer 42))))
             (Mul (Let "x" (integer 42) (Var "y"))
                  (Lt (Gt (integer 42) (Var "y")) 
                      (Let "x" (integer 12) (Eq (Var "y") (Var "x"))))))
    test "subst \"x\" 42 cond 1"
        (subst "x" (Integer 42)
            (Cond [ (Eq (Var "x") (integer 42), Var "x")
                  , (Eq (Var "x") (Var "x"), Var "x") 
                  , (Eq (Var "x") (Var "y"), Var "x") 
                  ] Nothing))
        (Cond [ (Eq (integer 42) (integer 42), integer 42)
              , (Eq (integer 42) (integer 42), integer 42) 
              , (Eq (integer 42) (Var "y"), integer 42) 
              ] Nothing)
    test "subst \"x\" 42 cond 2"
        (subst "x" (Integer 42)
            (Cond [ (Eq (Var "x") (integer 42), Var "x") ] 
                  (Just (Add (Var "x") (Var "y")))))
        (Cond [ (Eq (integer 42) (integer 42), integer 42) ] 
              (Just (Add (integer 42) (Var "y"))))
    test "subst \"x\" 42 (pair (right (pair x y)) (left (let (x x) (pair x z))))"
        (subst "x" (Integer 42) $
            Pair (PRight (Pair (Var "x") (Var "y")))
                  (PLeft (Let "x" (Var "x") (Pair (Var "x") (Var "z")))))
        (Pair (PRight (Pair (integer 42) (Var "y")))
              (PLeft (Let "x" (integer 42) (Pair (Var "x") (Var "z")))))


allTests :: IO ()
allTests = do
    testSection "evalProgram tests"
    test_evalProgram
    testSection "eval tests"
    test_eval
    testSection "subst tests"
    test_subst
