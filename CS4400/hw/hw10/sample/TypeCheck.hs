{-# LANGUAGE TupleSections #-}
{-|
Module      : TypeCheck
Description : A type-checker for protoScheme with type inference and let-polymorphism.
Copyright   : (c) Ferd, 2020
Maintainer  : f.vesely@northeastern

-}
module TypeCheck where

import Types
import Maps
import Syntax
import Result

import qualified SExpression as S

import SimpleTestsColor (test, testSection)

-- |Typing environments
type TyEnv = Map Variable Type

-- |Compute the type of expression
--
-- Returns `Failure` if the expression does not have a type.
typeOf :: TyEnv -> Expr -> Result Type
typeOf _ (Value v) = TyBase <$> typeOfValue v
  where
    typeOfValue :: Value -> Result BaseType
    typeOfValue (Integer _) = return TyInteger
    typeOfValue (Float _) = return TyReal
    typeOfValue (Boolean _) = return TyBoolean
    typeOfValue _ = 
        fail "Invalid value specification."
typeOf tenv (Var x) = 
    fromMaybe' ("Variable " ++ x ++ " is not defined") $
        get tenv x
typeOf tenv (Lam args body) = do
    ty <- typeOf (extendEnv tenv args) body
    let tys = map snd args
    return $ TyArrow $ tys ++ [ty]
typeOf tenv (Let x e1 e2) = do
    ty1 <- typeOf tenv e1
    typeOf (set tenv x ty1) e2
typeOf tenv (If e1 e2 e3) = do
    TyBase TyBoolean <- typeOf tenv e1
    ty2 <- typeOf tenv e2
    ty3 <- typeOf tenv e3
    if ty2 == ty3
        then return ty2
        else fail "if: branches need to have the same type"
typeOf tenv (And e1 e2) = do
    TyBase TyBoolean <- typeOf tenv e1
    TyBase TyBoolean <- typeOf tenv e2
    return $ TyBase TyBoolean
typeOf tenv (Or e1 e2) = do
    TyBase TyBoolean <- typeOf tenv e1
    TyBase TyBoolean <- typeOf tenv e2
    return $ TyBase TyBoolean
typeOf tenv (Cond clauses els) = do
    tys <- mapM checkClause clauses
    tys' <- addElse tys els
    (ty, _) <- nonEmpty tys'
    ensureEqual tys'
    return ty 
  where
    checkClause (con, e) = do
        TyBase TyBoolean <- typeOf tenv con
        typeOf tenv e
    nonEmpty [] = fail "Cond has to have at least one clause"
    nonEmpty (x : xs) = return (x, xs)
    ensureEqual [] = return ()
    ensureEqual (x : xs) 
        | all (x ==) xs = return ()
        | otherwise = fail "All clause expressions have to have the same type"
    addElse tys (Just e) = (: tys) <$> typeOf tenv e
    addElse tys _ = return tys
typeOf tenv (Call e es) = do
    TyArrow tys <- typeOf tenv e
    argTys <- mapM (typeOf tenv) es
    checkArgs argTys tys
  where
    checkArgs argTys tys 
        | length argTys == length tys - 1 && and (zipWith (==) argTys tys) 
              = return $ last tys
        | otherwise = fail "Formal and actual arguments do not match"
typeOf tenv (Pair e1 e2) = do
    ty1 <- typeOf tenv e1
    ty2 <- typeOf tenv e2
    return $ TyPair ty1 ty2
typeOf tenv (PLeft e) = do 
    TyPair ty1 _ <- typeOf tenv e
    return ty1
typeOf tenv (PRight e) = do
    TyPair _ ty2 <- typeOf tenv e
    return ty2


-- |Compute the type of the program.
--
-- Each global definition is checked against is signature. If the type of the 
-- definition's body does not match the type in the signature, the definition 
-- is rejected and the type-checking fails.
--
-- If any of the global definitions or the final expression fail to type-check
-- the program has no type and `Failure` is returned.
typeOfProgram :: TyEnv -> Program -> Result Type
typeOfProgram tenv (Program globals e) = do
    let tenv' = extendEnv tenv $ map getType globals
    mapM_ (checkGlobal tenv') globals
    typeOf tenv' e
  where
    getType (Define x ty _) = (x, ty)
    checkGlobal tenv (Define x ty e) = do
        ty' <- typeOf tenv e
        if ty == ty' 
            then return ty
            else fail $ "Signature " ++ x ++ " : " ++ showType ty 
                     ++ " does not match actual type " ++ showType ty'

-- | Type-check the program s-expressions.
typeOfProgramSExpr :: [S.Expr] -> Result S.Expr
typeOfProgramSExpr sexprs = do
    prog <- programFromSExpressions sexprs 
    typ <- typeOfProgram tyBase prog 
    return $ Types.toSExpression typ

-- | Extend an environment with a list of bindings
extendEnv :: TyEnv -> [(Variable, Type)] -> TyEnv
extendEnv = foldl $ \tenv (x, ty) -> set tenv x ty

-- | Base typing envnronment.
tyBase :: TyEnv
tyBase = fromList $
    arithOps ++ intComparisons ++
    [ ("not", TyArrow [TyBase TyBoolean, TyBase TyBoolean]) ]
  where
    arithOps = 
        map (, TyArrow $ replicate 3 $ TyBase TyInteger)
            ["+", "-", "*", "/"]
    intComparisons = 
        map (, TyArrow $ map TyBase [TyInteger, TyInteger, TyBoolean])
            ["<", ">", "<=", ">=", "="]


allTests :: IO ()
allTests = undefined
