{- |
Module      :  Church
Description :  Church encodings of values and operations.
               Fixpoint operator.
Copyright   : (c) Ferd, 2020
              (c) Kevin Zhang

Maintainer  : f.vesely@northeastern.edu 
              zhang.kevi@northeastern.edu
-}


module Church where

import Lambda
import Reduce

import SimpleTestsColor (test, testSection)


-- |Abbreviation to write a lambda with multiple arguments
lam :: [Variable] -> Lambda -> Lambda
lam = flip $ foldr Lam

app :: [Lambda] -> Lambda
app es | not $ null es = foldl1 App es

test_lam_app = do
  test "lam [x, y, z] $ app [x, y, z]"
      (lam ["x", "y", "z"] $ app [Var "x", Var "y", Var "z"])
      (Lam "x" $
          Lam "y" $
              Lam "z" $
                  App (App (Var "x") (Var "y"))
                      (Var "z"))

-- |Convert a boolean to its Church encoding
toChurchBool :: Bool -> Lambda
toChurchBool False = lam ["t", "f"] (Var "f")
toChurchBool True = lam ["t", "f"] (Var "t")

test_toChurchBool = do
  test "toChurchBool True"
    (toChurchBool False)
    (Lam "t" $ Lam "f" $ (Var "f"))
  test "toChurchBool False"
    (toChurchBool True)
    (Lam "t" $ Lam "f" $ (Var "t"))

-- |Convert a boolean from its Church encoding
fromChurchBool :: Lambda -> Maybe Bool
fromChurchBool (Lam a (Lam b (Var x)))
  | x == a = Just $ True
  | x == b = Just $ False
  | otherwise = Nothing
fromChurchBool _ = Nothing

test_fromChurchBool = do
  test "fromChurchBool (λ t f. t)"
    (fromChurchBool (Lam "t" $ Lam "f" $ (Var "t")))
    (Just $ True)
  test "fromChurchBool (λ t f. f)"
    (fromChurchBool (Lam "t" $ Lam "f" $ (Var "f")))
    (Just $ False)
  -- testing for variable-naming does not matter
  test "fromChurchBool (λ x y. x)"
    (fromChurchBool (Lam "x" $ Lam "y" $ (Var "x")))
    (Just $ True)
  -- testing for failure
  test "fromChurchBool (λ x y. z)"
    (fromChurchBool (Lam "x" $ Lam "y" $ (Var "z")))
    (Nothing)
  test "fromChurchBool (λ x. x)"
    (fromChurchBool (Lam "x" $ (Var "x")))
    (Nothing)

test_bools = do
  test_toChurchBool
  test_fromChurchBool


-- |Numerals
czero :: Lambda
czero = lam ["s", "z"] $ Var "z"

cone :: Lambda
cone = lam ["s", "z"] $ App (Var "s") (Var "z")

ctwo :: Lambda
ctwo = lam ["s", "z"] $ App (Var "s") (App (Var "s") (Var "z"))


-- |Generalized numeral: convert a given integer >= 0 to a Church numeral
toNumeral :: Integer -> Lambda
toNumeral n = lam ["s", "z"] $ (applyNtimes n (Var "s") (Var "z"))
  where
    applyNtimes 0 s z = z
    applyNtimes n s z = App s (applyNtimes (n - 1) s z)

test_toNumeral = do
  test "toNumeral 0"
    (toNumeral 0) czero
  test "toNumeral 1"
    (toNumeral 1) cone
  test "toNumeral 2"
    (toNumeral 2) ctwo

    
-- |Convert a normalized numeral into an integer. Return Nothing if the 
-- numeral cannot be converted.
fromNumeral :: Lambda -> Maybe Integer
fromNumeral (Lam s (Lam z (Var x)))
  | x == z = Just $ 0
  | otherwise = Nothing
fromNumeral (Lam s (Lam z (App (Var x) y)))
  | x == s = case fromNumeral (Lam s (Lam z y)) of
      Just i -> Just (i + 1)
      Nothing -> Nothing
  | otherwise = Nothing
fromNumeral _ = Nothing

test_fromNumeral = do
  test "fromNumeral (λ s z. z)"
    (fromNumeral czero) (Just 0)
  test "fromNumeral (λ s z. s z)"
    (fromNumeral cone) (Just 1)
  test "fromNumeral (λ s z. s s z)"
    (fromNumeral ctwo) (Just 2)
  -- testing for failure
  test "fromNumeral (λ x y. x)"
    (fromNumeral (Lam "x" $ Lam "y" $ (Var "x")))
    (Nothing)
  test "fromNumeral (λ s z. s z z)"
    (fromNumeral (Lam "s" $
                    Lam "z" $
                      App (Var "s") $
                        App (Var "z") (Var "z")))
    (Nothing)  
    
test_numerals = do
  test_toNumeral
  test_fromNumeral

-- |Successor of a natural number
csucc :: Lambda
csucc = lam ["n", "s", "z"] $
    App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))

test_csucc = do
  test "csucc ~1"
    (fromNumeral $ normalize $ (App csucc cone)) (Just 2) 
  test "csucc ~0"
    (fromNumeral $ normalize $ (App csucc czero)) (Just 1) 
  test "csucc ~2"
    (fromNumeral $ normalize $ (App csucc ctwo)) (Just 3)

-- |Predecessor of a natural number
cpred :: Lambda
cpred = Lam "n" (Lam "f" (Lam "x" (
          App (
            App (
              App (Var "n") 
                  (Lam "g" (Lam "h" (
                    App (Var "h") (App (Var "g") (Var "f")))))) 
              (Lam "u" (Var "x"))) 
            (Lam "u" (Var "u")))))

test_cpred = do
  test "cpred ~0"
    (fromNumeral $ normalize $ (App cpred czero)) (Just 0)
  test "cpred ~5"
    (fromNumeral $ normalize $ (App cpred (toNumeral 5))) (Just 4)
  test "cpred ~2"
    (fromNumeral $ normalize $ (App cpred ctwo)) (Just 1)

-- |Addition on naturals
cplus :: Lambda
cplus = lam ["m", "n"] $ Var "m" `App` csucc `App` Var "n"

-- some examples of cplus 
-- notation: I use ~n for Church numeral corresponding to n
test_cplus = do
    test "(cplus ~3 ~4)"
        (fromNumeral $ normalize $ 
            App (App cplus (toNumeral 3)) (toNumeral 4))
        (Just 7)

-- |Subtraction on naturals
cminus :: Lambda
cminus = lam ["m", "n"] $ Var "n" `App` cpred `App` Var "m"

test_cminus = do
  test "(cminus ~4 ~2)"
    (fromNumeral $ normalize $
        App (App cminus (toNumeral 4)) (toNumeral 2))
    (Just 2)
  test "(cminus ~4 ~4)"
    (fromNumeral $ normalize $
        App (App cminus (toNumeral 4)) (toNumeral 4))
    (Just 0)
  test "(cminus ~5 ~7)"
    (fromNumeral $ normalize $
        App (App cminus (toNumeral 5)) (toNumeral 7))
    (Just 0)

-- |Multiplication on naturals
ctimes :: Lambda
ctimes = lam ["m", "n"] $ (App (App (Var "m") (App cplus (Var "n"))) czero)  

test_ctimes = do
  test "(ctimes ~4 ~3)"
    (fromNumeral $ normalize $
        App (App ctimes (toNumeral 4)) (toNumeral 3))
    (Just 12)
  test "(ctimes ~0 ~2)"
    (fromNumeral $ normalize $
        App (App ctimes (toNumeral 0)) (toNumeral 2))
    (Just 0)
  test "(ctimes ~2 ~0)"
    (fromNumeral $ normalize $
        App (App ctimes (toNumeral 2)) (toNumeral 0))
    (Just 0)
  test "(ctimes ~5 ~5)"
    (fromNumeral $ normalize $
        App (App ctimes (toNumeral 5)) (toNumeral 5))
    (Just 25)

-- |Boolean and
cand :: Lambda
cand = lam ["m", "n"] $ (App (App (Var "m") (Var "n")) (toChurchBool False))

test_cand = do
  test "(cand ~#t ~#f)"
    (fromChurchBool $ normalize $
        App (App cand (toChurchBool True)) (toChurchBool False))
    (Just False)
  test "(cand ~#f ~#f)"
    (fromChurchBool $ normalize $
        App (App cand (toChurchBool False)) (toChurchBool False))
    (Just False)
  test "(cand ~#f ~#t)"
    (fromChurchBool $ normalize $
        App (App cand (toChurchBool False)) (toChurchBool True))
    (Just False)
  test "cand ~#t ~#t)"
    (fromChurchBool $ normalize $
        App (App cand (toChurchBool True)) (toChurchBool True))
    (Just True)

-- |Boolean or
cor :: Lambda 
cor = lam ["m", "n"] $ (App (App (Var "m") (toChurchBool True)) (Var "n"))

test_cor = do
  test "(cor ~#t ~#f)"
    (fromChurchBool $ normalize $
        App (App cor (toChurchBool True)) (toChurchBool False))
    (Just True)
  test "(cor ~#f ~#f)"
    (fromChurchBool $ normalize $
        App (App cor (toChurchBool False)) (toChurchBool False))
    (Just False)
  test "(cor ~#f ~#t)"
    (fromChurchBool $ normalize $
        App (App cor (toChurchBool False)) (toChurchBool True))
    (Just True)
  test "cor ~#t ~#t)"
    (fromChurchBool $ normalize $
        App (App cor (toChurchBool True)) (toChurchBool True))
    (Just True)

-- |Boolean negation
cnot :: Lambda
cnot = Lam "x" (App (App (Var "x") (toChurchBool False)) (toChurchBool True)) 

test_cnot = do
  test "(cnot ~#t)"
    (fromChurchBool $ normalize $ App cnot (toChurchBool True))
    (Just False)
  test "(cnot ~#f)"
    (fromChurchBool $ normalize $ App cnot (toChurchBool False))
    (Just True)

-- conditional expression
cifthen :: Lambda
cifthen = lam ["b", "t", "f"] $ Var "b" `App` Var "t" `App` Var "f"

test_cifthen = do
  test "(if ~#t ~1 ~2)"
    (fromNumeral $ normalize $
        App (App (App cifthen (toChurchBool True)) (toNumeral 1)) (toNumeral 2))
    (Just 1)
  test "(if ~#f ~1 ~2)"
    (fromNumeral $ normalize $
        App (App (App cifthen (toChurchBool False)) (toNumeral 1)) (toNumeral 2))
    (Just 2)
  -- using some clauses from below
  test "(if (ciszero ~0) ~1 ~2)"
    (fromNumeral $ normalize $
        App (App (App cifthen (App ciszero (toNumeral 0))) (toNumeral 1)) (toNumeral 2))
    (Just 1)
  test "(if (< ~0 ~2) ~1 ~2)"
    (fromNumeral $ normalize $
        App (App (App cifthen (App (App clt (toNumeral 0)) (toNumeral 2))) (toNumeral 1)) (toNumeral 2))
    (Just 1)
  test "(if (>= ~2 ~2) ~1 ~2)"
    (fromNumeral $ normalize $
        App (App (App cifthen (App (App cgt (toNumeral 2)) (toNumeral 2))) (toNumeral 1)) (toNumeral 2))
    (Just 2)
  

-- is a given numeral zero?
ciszero :: Lambda
ciszero = Lam "n" $ 
  Var "n" `App` (Lam "x" $ toChurchBool False) `App` (toChurchBool True)

test_ciszero = do
  test "(ciszero ~0)"
    (fromChurchBool $ normalize $
        App ciszero (toNumeral 0))
    (Just True)
  test "(ciszero ~1)"
    (fromChurchBool $ normalize $
        App ciszero (toNumeral 1))
    (Just False)
  test "(ciszero ~#t)"
    (fromChurchBool $ normalize $
        App ciszero (toChurchBool True))
    (Nothing)

-- |"less or equal"
cleq :: Lambda 
cleq = lam ["m", "n"] $ ciszero `App` (cminus `App` Var "m" `App` Var "n")

test_leq = do
  test "(<= ~0 ~1)"
    (fromChurchBool $ normalize $
        App (App cleq czero) cone)
    (Just True)
  test "(<= ~0 ~0)"
    (fromChurchBool $ normalize $
        App (App cleq czero) czero)
    (Just True)
  test "(<= ~2 ~1)"
    (fromChurchBool $ normalize $
        App (App cleq ctwo) cone)
    (Just False)

-- |"less than"
clt :: Lambda 
clt = lam ["m", "n"] $
    cand `App` (cleq `App` Var "m" `App` Var "n") 
         `App` (cnot `App` (ceq `App` Var "m" `App` Var "n"))

test_clt = do
  test "(< ~0 ~1)"
    (fromChurchBool $ normalize $
        App (App clt czero) cone)
    (Just True)
  test "(< ~0 ~0)"
    (fromChurchBool $ normalize $
        App (App clt czero) czero)
    (Just False)
  test "(< ~2 ~1)"
    (fromChurchBool $ normalize $
        App (App clt ctwo) cone)
    (Just False)

-- |"greater than"
cgt :: Lambda
cgt = lam ["m", "n"] $ cnot `App` (cleq `App` Var "m" `App` Var "n")

test_cgt = do
  test "(> ~0 ~1)"
    (fromChurchBool $ normalize $
        App (App cgt czero) cone)
    (Just False)
  test "(> ~0 ~0)"
    (fromChurchBool $ normalize $
        App (App cgt czero) czero)
    (Just False)
  test "(> ~2 ~1)"
    (fromChurchBool $ normalize $
        App (App cgt ctwo) cone)
    (Just True)

-- |"equal" for natural numbers
ceq :: Lambda
ceq = lam ["m", "n"] $
    cand `App` (cleq `App` Var "m" `App` Var "n")
         `App` (cleq `App` Var "n" `App` Var "m")

test_ceq = do
  test "(= ~0 ~1)"
    (fromChurchBool $ normalize $
        App (App ceq czero) cone)
    (Just False)
  test "(= ~0 ~0)"
    (fromChurchBool $ normalize $
        App (App ceq czero) czero)
    (Just True)
  test "(= ~2 ~1)"
    (fromChurchBool $ normalize $
        App (App ceq ctwo) cone)
    (Just False)
  test "(= ~#f ~0)" -- false and czero are identical ?
    (fromChurchBool $ normalize $
        App (App ceq (toChurchBool False)) czero)
    (Just True)
  test "(= ~#t ~1)"
    (fromChurchBool $ normalize $
        App (App ceq (toChurchBool True)) cone)
    (Just False)

-- |Pair constructor
cpair :: Lambda
cpair = lam ["l", "r", "s"] $ Var "s" `App` Var "l" `App` Var "r"

test_cpair = do
  test "(cpair ~0 ~1)"
    (normalize $ App (App cpair czero) cone)
    (Lam "s0" $ App
      (App (Var "s0") (Lam "s" $ Lam "z1" $ (Var "z1")))
      (Lam "s" $ Lam "z" $ (App (Var "s") (Var "z"))))
  test "(cpair ~#t (cpair ~#f ~2))"
    (normalize $ App (App cpair (toChurchBool True)) (App (App cpair (toChurchBool False)) ctwo))
    (Lam "s0" $ App
      (App (Var "s0") (Lam "t1" $ Lam "f" $ (Var "t1")))
      (Lam "s0" $ App
        (App (Var "s0") (Lam "t" $ Lam "f1" $ (Var "f1")))
        (Lam "s" $ Lam "z" $ (App (Var "s") (App (Var "s") (Var "z"))))))

-- |Left of a pair
cleft :: Lambda
cleft = Lam "p" $ Var "p" `App` lam ["l", "r"] (Var "l")

test_cleft = do
  test "(cleft (cpair ~0 ~1))"
    (fromNumeral $ normalize $
        App cleft (App (App cpair czero) cone))
    (Just 0)
  test "(cleft (cpair (cpair ~0 ~1) ~2))"
    (normalize $ App cleft $ App (App cpair (App (App cpair czero) cone)) ctwo)
    -- (normalize $ App (App cpair czero) cone)
    (Lam "s1" $ App
      (App (Var "s1") (Lam "s" (Lam "z1" (Var "z1"))))
      (Lam "s1" $ Lam "z1" $ (App (Var "s1") (Var "z1"))))
  test "(cleft (cleft (cpair (cpair ~0 ~1) ~2)))"
    (fromNumeral $ normalize $
        App cleft (App cleft (App (App cpair (App (App cpair czero) cone)) ctwo)))
    (Just 0)
    

-- |Right of a pair
cright :: Lambda
cright = Lam "p" $ Var "p" `App` lam ["l", "r"] (Var "r")

test_cright = do
  test "(cright (cpair ~0 ~1))"
    (fromNumeral $ normalize $
        App cright (App (App cpair czero) cone))
    (Just 1)
  test "(cright (cpair ~0 (cpair ~1 ~2)))"
    (normalize $ App cright $ App (App cpair czero) (App (App cpair cone) ctwo))
    -- (normalize $ App (App cpair cone) ctwo)
    (Lam "s1" $ App
      (App (Var "s1") (Lam "s0" (Lam "z1" (App (Var "s0") (Var "z1")))))
      (Lam "s1" (Lam "z1" (App (Var "s1") (App (Var "s1") (Var "z1"))))))
  test "(cright (cright (cpair ~0 (cpair ~1 ~2))))"
    (fromNumeral $ normalize $
        App cright (App cright (App (App cpair czero) (App (App cpair cone) ctwo))))
    (Just 2)

-- fixpoint combinator
cfix :: Lambda
cfix = Lam "f" $
                 (Lam "x" $ Var "f" `App` (Var "x" `App` Var "x")) 
           `App` (Lam "x" $ Var "f" `App` (Var "x" `App` Var "x"))

fact0 = (Lam "f" $ Lam "n" $
  App (App (App cifthen (App ciszero (Var "n")))
      cone)
      (App (App ctimes (App (Var "f") (App cpred (Var "n")))) (Var "n")))

pow0 = (Lam "f" $ Lam "n" $ Lam "e" $
  App (App (App cifthen (App ciszero (Var "e")))
      cone)
      (App (App ctimes (Var "n")) (App (App (Var "f") (Var "n")) (App cpred (Var "e")))))
                 
test_cfix = do
  test "(Y fact0) 3"
    (fromNumeral $ normalize $
        App (App cfix fact0) (toNumeral 3))
    (Just 6)
  -- NOTE: this takes a while ... 
  test "(Y pow0) 2 3"
    (fromNumeral $ normalize $
          App (App (App cfix pow0) (toNumeral 2)) (toNumeral 3))
    (Just 8)
  

allTests :: IO ()
allTests = do
    test_lam_app
    test_bools
    test_numerals
    test_csucc
    test_cpred
    test_cplus
    test_cminus
    test_ctimes
    test_cand
    test_cor
    test_cnot
    test_cifthen
    test_ciszero
    test_leq
    test_clt
    test_cgt
    test_ceq
    test_cpair
    test_cleft
    test_cright
    test_cfix

