{-# LANGUAGE TupleSections #-}
{-|
Module      : TypeCheck
Description : A type-checker for protoScheme with type inference and let-polymorphism.
Copyright   : (c) Ferd, 2020
Maintainer  : f.vesely@northeastern

An implementation of the protoScheme type-checker.

-}
module TypeCheck where

import Types
import Maps
import Syntax
import Result

import qualified SExpression as S

import SimpleTestsColor (test, testSection)

import Control.Monad.Fail
import Prelude hiding (fail)

-- |Type environments
type TyEnv = Map Variable Type

-- |Compute the type of the given expression
typeOf :: TyEnv -> Expr -> Result Type
typeOf _ (Value v) = TyBase <$> typeOfValue v
  where
    typeOfValue :: Value -> Result BaseType
    typeOfValue (Integer _) = return TyInteger
    typeOfValue (Float _) = return TyReal
    typeOfValue (Boolean _) = return TyBoolean
    typeOfValue _ = fail "Invalid value."
typeOf tenv (Var x) = 
    fromMaybe' ("Variable " ++ x ++ " is not defined") $
        get tenv x
typeOf tenv (Lam sigs body) = do
  tys <- return $ map (\sig -> getTy sig) sigs
  tybody <- typeOf (addVars tenv sigs) body
  return $ TyArrow (tys ++ [tybody])
  where
    addVars :: TyEnv -> [Signature] -> TyEnv
    addVars e [] = e
    addVars e ((TySig x t) : sigs) = addVars (set tenv x t) sigs
typeOf tenv (Call (Var "pair") [p1, p2]) = do
  t1 <- typeOf tenv p1
  t2 <- typeOf tenv p2
  return $ TyPair t1 t2
typeOf tenv (Call (Var "left") [e]) = do
  t <- typeOf tenv e
  case t of
    (TyPair p1 p2) -> return $ p1
    _ -> fail "Left has not ben called on a pair"
typeOf tenv (Call (Var "right") [e]) = do
  t <- typeOf tenv e
  case t of
    (TyPair p1 p2) -> return $ p2
    _ -> fail "Right has not been called on a pair"  
typeOf tenv (Call (Var "pair") _) = fail "Pair express does not match expected pair type"
typeOf tenv (Call (Var "left") _) = fail "Left has not been called on a pair"
typeOf tenv (Call (Var "right") _) = fail "Right has not been called on a pair"
typeOf tenv (Call e1 e2) = do
  TyArrow tys <- typeOf tenv e1
  accepted <- compareTypes $ zip tys e2
  if accepted
    then return (last tys)
    else fail "Argument types do not match"
  where
    compareTypes :: [(Type, Expr)] -> Result Bool
    compareTypes [] = return True
    compareTypes ((t1, t2) : ts) = do
      t2' <- typeOf tenv t2
      if (t1 == t2')
        then compareTypes ts
        else return False
typeOf tenv (Cond clauses e) = do
  ensured <- ensureAllBools clauses
  tys <- collectTypes clauses
  case e of
    (Just expr) -> do
      tyE <- typeOf tenv expr
      ensureSameTypes (tys ++ [tyE])
    Nothing -> do
      ensureSameTypes tys
  where
    -- |Ensures that the left-hand side of each cond clause is a TyBoolean
    ensureAllBools :: [Clause] -> Result Bool
    ensureAllBools [] = Success $ True
    ensureAllBools ((e1, _):rest) = do
      t1 <- typeOf tenv e1
      if (t1 == (TyBase TyBoolean))
        then ensureAllBools rest
        else fail "Cond clauses are missing predicates"

    -- |Collects the types of the right-hand sides of each cond clause
    collectTypes :: [Clause] -> Result [Type]
    collectTypes [] = Success $ []
    collectTypes ((_, e2):rest) = do
      t <- typeOf tenv e2
      rest' <- collectTypes rest
      return (t:rest')

    -- |Ensures that all the types in a list are the same type, and returns that type
    ensureSameTypes :: [Type] -> Result Type
    ensureSameTypes [ty] = Success $ ty
    ensureSameTypes (t1 : t2 : tys) | t1 == t2 = ensureSameTypes (t2 : tys)
                                    | otherwise = fail "Cond types do not match"
typeOf tenv (Let x e1 e2) = do
  t1 <- typeOf tenv e1
  typeOf (set tenv x t1) e2
typeOf tenv (If pred e1 e2) = do
  pType <- typeOf tenv pred
  t1 <- typeOf tenv e1
  t2 <- typeOf tenv e2
  if (pType == (TyBase TyBoolean)) then
    (if (t1 == t2)
      then return $ t1
      else fail "If expression types do not match")
    else fail "If predicate is not a boolean"
typeOf tenv (And e1 e2) = do
  t1 <- typeOf tenv e1
  t2 <- typeOf tenv e2
  if t1 == (TyBase TyBoolean) && t2 == (TyBase TyBoolean)
    then return $ TyBase TyBoolean
    else fail "And types are not boolean"
typeOf tenv (Or e1 e2) = do
  t1 <- typeOf tenv e1
  t2 <- typeOf tenv e2
  if t1 == (TyBase TyBoolean) && t2 == (TyBase TyBoolean)
    then return $ TyBase TyBoolean
    else fail "Or types are not boolean"
-- typeOf tenv _ = undefined

-- |Pulls type from a type signature
getTy :: Signature -> Type
getTy (TySig s t) = t

-- |Compute the type of the given program, relative to the given type environment
typeOfProgram :: TyEnv -> Program -> Result Type
typeOfProgram tenv (Program defs e) = do
  tenv' <- return $ addGlobalSignatures tenv defs
  typeOf tenv' e
  where
    addGlobalSignatures :: TyEnv -> [GlobalDef] -> TyEnv
    addGlobalSignatures tenv [] = tenv
    addGlobalSignatures tenv ((Define (TySig f ty) _):gdfs) =
      addGlobalSignatures (set tenv f ty) gdfs  

-- |Compute the 
typeOfProgramSExpr :: [S.Expr] -> Result S.Expr
typeOfProgramSExpr sexprs = do
    prog <- programFromSExpressions sexprs
    typ <- typeOfProgram tyBase prog 
    return $ Types.toSExpression typ

tyBase = fromList 
    [ ("+", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger] )
    , ("-", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger] )
    , ("*", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger] )
    , ("/", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger] )
    , ("<", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyBoolean] )
    , (">", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyBoolean] )
    , ("<=", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyBoolean] )
    , (">=", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyBoolean] )
    , ("=", TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyBoolean] )
    , ("not", TyArrow [TyBase TyBoolean, TyBase TyBoolean] )
    ]

allTests = do
    testSection "typeOf"
    test_typeOf
    testSection "typeOfProgram"
    test_typeOfProgram

test_typeOf = do
  test "typeOf 42"
    (typeOf empty (integer 42))
    (Success $ TyBase TyInteger)
  test "typeOf #t"
    (typeOf empty (Value $ Boolean True))
    (Success $ TyBase TyBoolean)
  test "typeOf x"
    (typeOf empty (Var "x"))
    (Failure $ "Variable x is not defined")
  test "typeOf y (defined)"
    (typeOf (set empty "x" (TyBase TyReal)) (Var "x"))
    (Success $ TyBase TyReal)
  test "typeOf (lambda (x : Integer) x)"
    (typeOf empty (Lam [TySig "x" (TyBase TyInteger)] (Var "x")))
    (Success $ TyArrow [TyBase TyInteger, TyBase TyInteger])
  test "typeOf ((lambda (x : Integer) x) 42)"
    (typeOf empty (Call (Lam [TySig "x" (TyBase TyInteger)] (Var "x")) [integer 42]))
    (Success $ TyBase TyInteger)
  test "typeOf ((lambda (x : Integer) x) #t)"
    (typeOf empty (Call (Lam [TySig "x" (TyBase TyInteger)] (Var "x")) [Value $ Boolean True]))
    (Failure $ "Argument types do not match")
  test "typeOf (cond [(#t 42) (#f 32)])"
    (typeOf empty (Cond [(Value $ Boolean True, integer 42), (Value $ Boolean False, integer 32)] Nothing))
    (Success $ TyBase TyInteger)
  test "typeOf (cond [(#t #t) (#f 32)] 42)"
    (typeOf empty (Cond [(Value $ Boolean True, Value $ Boolean True),
                         (Value $ Boolean False, integer 32)]
                    (Just (integer 42))))
    (Failure $ "Cond types do not match")
  test "typeOf (cond [(42 32)])"
    (typeOf empty (Cond [(integer 42, integer 32)] Nothing))
    (Failure $ "Cond clauses are missing predicates")
  test "typeOf (let x 42 x)"
    (typeOf empty (Let "x" (integer 42) (Var "x")))
    (Success $ TyBase TyInteger)
  test "typeOf (let x 42 #t)"
    (typeOf empty (Let "x" (integer 42) (Value $ Boolean True)))
    (Success $ TyBase TyBoolean)
  test "typeOf (if #t #f #t)"
    (typeOf empty (If (Value $ Boolean True) (Value $ Boolean False) (Value $ Boolean True)))
    (Success $ TyBase TyBoolean)
  test "typeOf (if 42 32 31)"
    (typeOf empty (If (integer 42) (integer 32) (integer 31)))
    (Failure $ "If predicate is not a boolean")
  test "typeOf (if #t 42 #f)"
    (typeOf empty (If (Value $ Boolean True) (integer 42) (Value $ Boolean False)))
    (Failure $ "If expression types do not match")
  test "typeOf (and (< 2 1) (not #t))"
    (typeOf tyBase (And
                    (Call (Var "<") [(integer 2), (integer 1)])
                    (Call (Var "not") [(Value $ Boolean True)])))
    (Success $ TyBase TyBoolean)
  test "typeOf (or 42 #t)"
    (typeOf tyBase (Or (integer 42) (Value $ Boolean True)))
    (Failure $ "Or types are not boolean")
  test "typeOf (not 42)"
    (typeOf tyBase (Call (Var "not") [(integer 42)]))
    (Failure $ "Argument types do not match")
  test "typeOf (not #t)"
    (typeOf tyBase (Call (Var "not") [(Value $ Boolean True)]))
    (Success $ TyBase TyBoolean)
  test "typeOf (+ 1 (- 20 (* 3 (/ 4 3))))"
    (typeOf tyBase (Call (Var "+")
                     [ (integer 1)
                     , Call (Var "-")
                      [ (integer 20)
                      , Call (Var "*")
                        [ (integer 3)
                        , Call (Var "/")
                          [ (integer 4), (integer 3)]]]]))
    (Success $ TyBase TyInteger)
  test "typeOf (and (>= 2 1) (<= 3 #t))"
    (typeOf tyBase (And
                     (Call (Var ">=") [(integer 2), (integer 1)])
                     (Call (Var "<=") [(integer 3), (Value $ Boolean True)])
                   ))
    (Failure $ "Argument types do not match")
  test "typeOf (lambda (x : Integer) (cond [(> 1 x) x] 4)"
    (typeOf tyBase (Lam [TySig "x" (TyBase TyInteger)]
                    (Cond
                      [(Call (Var ">") [(integer 1), (Var "x")], (Var "x"))]
                      (Just $ (integer 4))
                    )))
    (Success $ TyArrow [TyBase TyInteger, TyBase TyInteger])
  test "typeOf (pair 1 #t)"
    (typeOf tyBase (Call (Var "pair") [(integer 1), (Value $ Boolean True)]))
    (Success $ TyPair (TyBase TyInteger) (TyBase TyBoolean))
  test "typeOf (left (pair (+ 1 2) 3.14))"
    (typeOf tyBase (Call (Var "left")
                    [Call (Var "pair") [(Call (Var "+") [(integer 1), (integer 2)]), Value $ Float 3.14]]))
    (Success $ TyBase TyInteger)
  test "typeOf (right (and #t #f))"
    (typeOf tyBase (Call (Var "right") [(And (Value $ Boolean True) (Value $ Boolean False))]))
    (Failure $ "Right has not been called on a pair")

test_typeOfProgram = do
  test "global variable definition and lookup"
    (typeOfProgram tyBase $ Program [Define (TySig "x" (TyBase TyInteger)) $ integer 12] (Var "x"))
    (Success $ TyBase TyInteger)
  test "use a global in a definition"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
              , Define (TySig "y" (TyBase TyInteger)) $ Call (Var "+") [Var "x", Var "x"]
              ] $
      Var "y")
    (Success $ TyBase TyInteger)
  test "simple function"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                $ Lam [TySig "x" (TyBase TyInteger)] (Var "x") ] $
      Call (Var "f") [integer 12])
    (Success $ TyBase TyInteger)
  test "multiple arguments 1"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger, TyBase TyInteger]))
                $ Lam [TySig "x" (TyBase TyInteger), TySig "y" (TyBase TyInteger), TySig "z" (TyBase TyInteger)] (Var "y") ] $
      Call (Var "f") $ map integer [1, 2, 3])
    (Success $ TyBase TyInteger)
  test "multiple arguments 2"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger, TyBase TyInteger]))
                $ Lam [TySig "x" (TyBase TyInteger), TySig "y" (TyBase TyInteger), TySig "z" (TyBase TyInteger)] (Var "z") ] $
      Call (Var "f") $ map integer [1, 2, 3])
    (Success $ TyBase TyInteger)
  test "use a global in a function definition"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
              , Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                $ Lam [TySig "y" (TyBase TyInteger)] $ Call (Var "+") [Var "x", Var "y"]
              ] $
      Call (Var "f") [ Call (Var "-") [ integer 0, Var "x" ] ])
    (Success $ TyBase TyInteger)
  test "recursive function (factorial)"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                $ Lam [TySig "x" (TyBase TyInteger)] $
                If (Call (Var "<") [ Var "x", integer 2])
                (integer 1)
                (Call (Var "*") 
                  [ Var "x"
                  , Call (Var "f") 
                    [ Call (Var "-") 
                      [ Var "x"
                      , integer 1 
                      ] 
                    ] 
                  ])
              ] $
      Call (Var "f") [ integer 10 ])
    (Success $ TyBase TyInteger)
  test "local shodowing 1"
    (typeOfProgram tyBase $
      Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12 ] $
      Let "x" (integer 42) (Var "x"))
    (Success $ TyBase TyInteger)
  test "local shodowing 2"
    (typeOfProgram tyBase $
      Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12 
              , Define (TySig "y" (TyBase TyInteger)) $ Call (Var "+") [Var "x", integer 1]
              ] $
      Let "x" (Call (Var "-") [Var "x", integer 1]) 
      (Call (Var "pair") [Var "x", Var "y"]))
    (Success $ TyPair (TyBase TyInteger) (TyBase TyInteger))
  test "global overriding type-checks"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
              , Define (TySig "x" (TyBase TyInteger)) $ integer 13
              ]
      (Var "x"))
    (Success $ TyBase TyInteger)
  
