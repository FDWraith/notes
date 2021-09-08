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
import Gensym

import qualified SExpression as S

import SimpleTestsColor (test, testSection)

import Control.Monad.Fail
import Prelude hiding (fail)

-- |Type environments
type TyEnv = Map Variable Type

-- |Compute the type of the given expression
typeOf :: TyEnv -> Expr -> Result (Type, [Constraint])
typeOf _ (Value v) = do
  v' <- typeOfValue v
  return (TyBase v', [])
  where
    typeOfValue :: Value -> Result BaseType
    typeOfValue (Integer _) = return TyInteger
    typeOfValue (Float _) = return TyReal
    typeOfValue (Boolean _) = return TyBoolean
    typeOfValue _ = fail "Invalid value."
typeOf tenv (Var x) = do
    ty <- fromMaybe' ("Variable " ++ x ++ " is not defined") $ get tenv x
    return (ty, [])
typeOf tenv (Lam vars body) = do
  -- tys <- return $ map (\sig -> getTy sig) sigs
  tys <- return $ map (\var -> TyVar $ gensym var) vars 
  (t', c') <- typeOf (addVars tenv (zip vars tys)) body
  return $ (TyArrow (tys ++ [t']), c')
  where
    addVars :: TyEnv -> [(Variable, Type)] -> TyEnv
    addVars e [] = e
    addVars e ((x, ty) : vars) = addVars (set e x ty) vars
typeOf tenv Nil = return $ (TyList (TyVar $ gensym "l"), [])
typeOf tenv (Cons e1 e2) = do
  (lT, lC) <- typeOf tenv e2
  (iT, iC) <- typeOf tenv e1
  let l = TyVar $ gensym "l"
  return $ (lT, [(lT, TyList l), (iT, l)] ++ lC ++ iC)
typeOf tenv (Call (Var "pair") [p1, p2]) = do
  (t1, c1) <- typeOf tenv p1
  (t2, c2) <- typeOf tenv p2
  return $ (TyPair t1 t2, c1 ++ c2)
typeOf tenv (Call (Var "left") [e]) = do
  (t, c) <- typeOf tenv e
  let p1 = TyVar $ gensym "p"
  let p2 = TyVar $ gensym "p"
  return $ (p1, [(TyPair p1 p2, t)] ++ c)
  --(t, c) <- typeOf tenv e
  --case t of
  --  (TyPair p1 p2) -> return $ (p1, c)
  --  _ -> fail "Left has not been called on a pair"
typeOf tenv (Call (Var "right") [e]) = do
  (t, c) <- typeOf tenv e
  let p1 = TyVar $ gensym "p"
  let p2 = TyVar $ gensym "p"
  return $ (p2, [(TyPair p1 p2, t)] ++ c)
  --case t of
  --  (TyPair p1 p2) -> return $ (p2, c)
   -- _ -> fail "Right has not been called on a pair"
typeOf tenv (Call (Var "head") [e]) = do
  (t', c') <- typeOf tenv e
  let l = TyVar $ gensym "l"
  return $ (l, [(TyList l, t')] ++ c')
typeOf tenv (Call (Var "tail") [e]) = do
  (t', c') <- typeOf tenv e
  let l = TyVar $ gensym "l"
  return $ (TyList l, [(TyList l, t')] ++ c')
typeOf tenv (Call (Var "cons?") _) = do
  return $ (TyBase TyBoolean, [])
typeOf tenv (Call (Var "nil?") _) = do
  return $ (TyBase TyBoolean, [])
typeOf tenv (Call (Var "list?") _) = do
  return $ (TyBase TyBoolean, [])
typeOf tenv (Call e1 e2) = do
  (t1, c1) <- typeOf tenv e1
  tys <- mapM (\e -> typeOf tenv e) e2
  (ts, cs) <- return $ unzip tys
  let tRet = TyVar $ gensym "r"
  return (tRet, (t1, TyArrow (ts ++ [tRet])) : (concat cs))
typeOf tenv (Cond clauses e) = do
  (ts, cs) <- collectTypes clauses
  case e of
    Just e -> do
      (te, ce) <- typeOf tenv e
      return (te, (ensureAll (te : ts)) ++ cs)
    Nothing -> do
      return (head ts, (ensureAll ts) ++ cs)
  where
    collectTypes :: [Clause] -> Result ([Type], [Constraint])
    collectTypes [] = return ([], [])
    collectTypes ((e1, e2) : rest) = do
      (t1, c1) <- typeOf tenv e1
      (t2, c2) <- typeOf tenv e2
      (ts, cs) <- collectTypes rest
      return (t2 : ts, [(TyBase TyBoolean, t1)] ++ cs ++ c1 ++ c2)

    ensureAll :: [Type] -> [Constraint]
    ensureAll [] = []
    ensureAll (t1 : []) = []
    ensureAll (t1 : t2 : rest) = (t1, t2) : (ensureAll (t2:rest))
typeOf tenv (Let x e1 e2) = do
  (t1, c1) <- typeOf tenv e1
  (t2, c2) <- typeOf (set tenv x t1) e2
  return $ (t2, c1 ++ c2)
typeOf tenv (If pred e1 e2) = do
  (p, cp) <- typeOf tenv pred
  (t1, c1) <- typeOf tenv e1
  (t2, c2) <- typeOf tenv e2
  return $ (t1, [(TyBase TyBoolean, p), (t1, t2)] ++ cp ++ c1 ++ c2)
typeOf tenv (And e1 e2) = do
  (t1, c1) <- typeOf tenv e1
  (t2, c2) <- typeOf tenv e2
  return $ (TyBase TyBoolean, [(TyBase TyBoolean, t1), (TyBase TyBoolean, t2)] ++ c1 ++ c2)
typeOf tenv (Or e1 e2) = do
  (t1, c1) <- typeOf tenv e1
  (t2, c2) <- typeOf tenv e2
  return $ (TyBase TyBoolean, [(TyBase TyBoolean, t1), (TyBase TyBoolean, t2)] ++ c1 ++ c2)
-- typeOf tenv _ = fail "unknown expression"

-- |Pulls type from a type signature
getTy :: Signature -> Type
getTy (TySig s t) = t

-- |Compute the type of the given program, relative to the given type environment
typeOfProgram :: TyEnv -> Program -> Result Type
typeOfProgram tenv (Program defs e) = do
  tenv' <- return $ addGlobalSignatures tenv defs
  gC <- globalConstraints tenv' defs
  (t, c) <- typeOf tenv' e
  s <- unify (c ++ gC)
  return $ applySubst s t
  where
    addGlobalSignatures :: TyEnv -> [GlobalDef] -> TyEnv
    addGlobalSignatures tenv [] = tenv
    addGlobalSignatures tenv ((Define (TySig f ty) _):gdfs) =      
      addGlobalSignatures (set tenv f ty) gdfs

    globalConstraints :: TyEnv -> [GlobalDef] -> Result [Constraint]
    globalConstraints tenv [] = return $ []
    globalConstraints tenv ((Define (TySig f ty) body):gdfs) = do
      (bT, bC) <- typeOf tenv body
      bTs <- unify bC
      bT' <- return $ applySubst bTs bT
      rest <- globalConstraints tenv gdfs
      if specializes ty bT'
        then return $ [(ty, bT')] ++ bC ++ rest
        else fail $ "Function Signature should match Function Type" 
      
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
    (typeOfP empty (integer 42))
    (Success $ TyBase TyInteger)
  test "typeOf #t"
    (typeOfP empty (Value $ Boolean True))
    (Success $ TyBase TyBoolean)
  test "typeOf x"
    (typeOfP empty (Var "x"))
    (Failure $ "Variable x is not defined")
  test "typeOf y (defined)"
    (typeOfP (set empty "x" (TyBase TyReal)) (Var "x"))
    (Success $ TyBase TyReal)
  test "typeOf (lambda (x) x)"
    (typeOfP empty (Lam ["x"] (Var "x")))
    (Success $ TyArrow [TyVar "x#1", TyVar "x#1"])
  test "typeOf ((lambda (x) x) 42)"
    (typeOfP empty (Call (Lam ["x"] (Var "x")) [integer 42]))
    (Success $ TyBase TyInteger)
  test "typeOf ((lambda (x : Integer) x) #t)"
    (typeOfP empty (Call (Lam ["x"] (Var "x")) [Value $ Boolean True]))
    (Success $ TyBase TyBoolean)
  test "typeOf (cond [(#t 42) (#f 32)])"
    (typeOfP empty (Cond [(Value $ Boolean True, integer 42), (Value $ Boolean False, integer 32)] Nothing))
    (Success $ TyBase TyInteger)
  test "typeOf (cond [(#t #t) (#f 32)] 42)"
    (typeOfP empty (Cond [(Value $ Boolean True, Value $ Boolean True),
                         (Value $ Boolean False, integer 32)]
                    (Just (integer 42))))
    (Failure $ "Could not unify Integer and Boolean")
  test "typeOf (cond [(42 32)])"
    (typeOfP empty (Cond [(integer 42, integer 32)] Nothing))
    (Failure $ "Could not unify Boolean and Integer")
  test "typeOf (let x 42 x)"
    (typeOfP empty (Let "x" (integer 42) (Var "x")))
    (Success $ TyBase TyInteger)
  test "typeOf (let x 42 #t)"
    (typeOfP empty (Let "x" (integer 42) (Value $ Boolean True)))
    (Success $ TyBase TyBoolean)
  test "typeOf (if #t #f #t)"
    (typeOfP empty (If (Value $ Boolean True) (Value $ Boolean False) (Value $ Boolean True)))
    (Success $ TyBase TyBoolean)
  test "typeOf (if 42 32 31)"
    (typeOfP empty (If (integer 42) (integer 32) (integer 31)))
    (Failure $ "Could not unify Boolean and Integer")
  test "typeOf (if #t 42 #f)"
    (typeOfP empty (If (Value $ Boolean True) (integer 42) (Value $ Boolean False)))
    (Failure $ "Could not unify Integer and Boolean")
  test "typeOf (and (< 2 1) (not #t))"
    (typeOfP tyBase (And
                    (Call (Var "<") [(integer 2), (integer 1)])
                    (Call (Var "not") [(Value $ Boolean True)])))
    (Success $ TyBase TyBoolean)
  test "typeOf (or 42 #t)"
    (typeOfP tyBase (Or (integer 42) (Value $ Boolean True)))
    (Failure $ "Could not unify Boolean and Integer")
  test "typeOf (not 42)"
    (typeOfP tyBase (Call (Var "not") [(integer 42)]))
    (Failure $ "Could not unify Boolean and Integer")
  test "typeOf (not #t)"
    (typeOfP tyBase (Call (Var "not") [(Value $ Boolean True)]))
    (Success $ TyBase TyBoolean)
  test "typeOf (+ 1 (- 20 (* 3 (/ 4 3))))"
    (typeOfP tyBase (Call (Var "+")
                     [ (integer 1)
                     , Call (Var "-")
                      [ (integer 20)
                      , Call (Var "*")
                        [ (integer 3)
                        , Call (Var "/")
                          [ (integer 4), (integer 3)]]]]))
    (Success $ TyBase TyInteger)
  test "typeOf (and (>= 2 1) (<= 3 #t))"
    (typeOfP tyBase (And
                     (Call (Var ">=") [(integer 2), (integer 1)])
                     (Call (Var "<=") [(integer 3), (Value $ Boolean True)])
                   ))
    (Failure $ "Could not unify Integer and Boolean")
  test "typeOf (lambda (x : Integer) (cond [(> 1 x) x] 4)"
    (typeOfP tyBase (Lam ["x"]
                    (Cond
                      [(Call (Var ">") [(integer 1), (Var "x")], (Var "x"))]
                      (Just $ (integer 4))
                    )))
    (Success $ TyArrow [TyBase TyInteger, TyBase TyInteger])
  test "typeOf (pair 1 #t)"
    (typeOfP tyBase (Call (Var "pair") [(integer 1), (Value $ Boolean True)]))
    (Success $ TyPair (TyBase TyInteger) (TyBase TyBoolean))
  test "typeOf (left (pair (+ 1 2) 3.14))"
    (typeOfP tyBase (Call (Var "left")
                    [Call (Var "pair") [(Call (Var "+") [(integer 1), (integer 2)]), Value $ Float 3.14]]))
    (Success $ TyBase TyInteger)
  test "typeOf (right (and #t #f))"
    (typeOfP tyBase (Call (Var "right") [(And (Value $ Boolean True) (Value $ Boolean False))]))
    (Failure $ "Could not unify (Pair-of p#20 p#21) and Boolean")
  test "typeOf (cons nil nil)"
    (typeOfP tyBase (Cons Nil Nil))
    (Success $ TyList $ TyList $ TyVar "l#24")
  test "typeOf nil"
    (typeOfP empty (Nil))
    (Success $ TyList $ TyVar "l#25")
  test "typeOf (cons 1 (cons #t nil))"
    (typeOfP empty (Cons (integer 1) (Cons (Value $ Boolean True) Nil)))
    (Failure $ "Could not unify Boolean and Integer")
  test "typeOf (head (cons 1 nil))"
    (typeOfP empty (Call (Var "head") [(Cons (integer 1) Nil)]))
    (Success $ TyBase TyInteger)
  test "typeOf (tail (cons 1 (cons 2 (cons 3 nil))))"
    (typeOfP empty (Call (Var "tail") [(Cons (integer 1) (Cons (integer 2) (Cons (integer 3) Nil)))]))
    (Success $ TyList $ TyBase TyInteger)
  test "typeOf (list? 1)"
    (typeOfP empty (Call (Var "list?") [(integer 1)]))
    (Success $ TyBase TyBoolean)
  test "typeOf (cons? nil)"
    (typeOfP empty (Call (Var "cons?") [Nil]))
    (Success $ TyBase TyBoolean)
  where
    typeOfP tenv expr = typeOfProgram tenv (Program [] expr)

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
                $ Lam ["x"] (Var "x") ] $
      Call (Var "f") [integer 12])
    (Success $ TyBase TyInteger)
  test "multiple arguments 1"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger, TyBase TyInteger]))
                $ Lam ["x", "y", "z"] (Var "y") ] $
      Call (Var "f") $ map integer [1, 2, 3])
    (Success $ TyBase TyInteger)
  test "multiple arguments 2"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger, TyBase TyInteger]))
                $ Lam ["x", "y", "z"] (Var "z") ] $
      Call (Var "f") $ map integer [1, 2, 3])
    (Success $ TyBase TyInteger)
  test "use a global in a function definition"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "x" (TyBase TyInteger)) $ integer 12
              , Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                $ Lam ["y"] $ Call (Var "+") [Var "x", Var "y"]
              ] $
      Call (Var "f") [ Call (Var "-") [ integer 0, Var "x" ] ])
    (Success $ TyBase TyInteger)
  test "recursive function (factorial)"
    (typeOfProgram tyBase $ 
      Program [ Define (TySig "f" (TyArrow [TyBase TyInteger, TyBase TyInteger]))
                $ Lam ["x"] $
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
  test "incorrect global typing"
    (typeOfProgram tyBase $
      Program [ Define (TySig "blah" (TyArrow [TyBase TyBoolean, TyBase TyBoolean, TyBase TyBoolean])) $
                Lam ["x", "y"] (Call (Var "+") [Var "x", Var "y"])    
              ] $
      (Var "blah"))
    (Failure $ "Function Signature should match Function Type")
  test "correct global typing"
    (typeOfProgram tyBase $
      Program [ Define (TySig "blah" (TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger])) $
                Lam ["x", "y"] (Call (Var "+") [Var "x", Var "y"])    
              ] $
      (Var "blah"))
    (Success $ TyArrow [TyBase TyInteger, TyBase TyInteger, TyBase TyInteger])
  test "variable global typing"
    (typeOfProgram tyBase $
      Program [ Define (TySig "blah" (TyArrow [TyVar "a", TyVar "a", TyVar "a"])) $
                Lam ["x", "y"] (Call (Var "+") [Var "x", Var "y"])    
              ] $
      (Var "blah"))
    (Failure $ "Function Signature should match Function Type")
  test "Specified global typing"
    (typeOfProgram tyBase $
      Program [ Define (TySig "blah" (TyArrow [TyBase TyInteger, TyBase TyInteger])) $
                Lam ["x"] (Var "x")    
              ] $
      (Var "blah"))
    (Success $ TyArrow [TyBase TyInteger, TyBase TyInteger])
