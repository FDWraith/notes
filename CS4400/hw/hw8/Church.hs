{- |
Module      :  Church
Description :  Church encodings of values and operations.
               Fixpoint operator.
Copyright   : (c) Ferd, 2020
              (c) <your names>

Maintainer  : f.vesely@northeastern.edu 
              <your emails>
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
toChurchBool True = lam ["t", "f"] $ Var "t"
toChurchBool False = lam ["t", "f"] $ Var "f"

-- |Convert a boolean from its Church encoding
fromChurchBool :: Lambda -> Maybe Bool
fromChurchBool (Lam t (Lam f (Var t'))) 
    | t == t' && t /= f = Just True
fromChurchBool (Lam _ (Lam f (Var f'))) 
    | f == f' = Just False
fromChurchBool _ = Nothing

test_bools = do
    test "toChurchBool True" (toChurchBool True) (lam ["t", "f"] $ Var "t")
    test "toChurchBool False" (toChurchBool False) (lam ["t", "f"] $ Var "f")
    test "fromChurchBool (λx y. y)" 
        (fromChurchBool $ lam ["x", "y"] $ Var "y")
        (Just False)
    test "fromChurchBool (λfoo bar. foo)" 
        (fromChurchBool $ lam ["foo", "bar"] $ Var "foo")
        (Just True)
    test "fromChurchBool (λfoo foo. foo)" 
        (fromChurchBool $ lam ["foo", "foo"] $ Var "foo")
        (Just False)
    test "fromChurchBool (λfoo. foo)" 
        (fromChurchBool $ lam ["foo"] $ Var "foo")
        Nothing
    test "fromChurchBool (λfoo bar baz. foo)" 
        (fromChurchBool $ lam ["foo", "bar", "baz"] $ Var "bar")
        Nothing
    test "fromChurchBool (foo)" 
        (fromChurchBool $ Var "foo")
        Nothing

-- |Numerals
czero :: Lambda
czero = lam ["s", "z"] $ Var "z"

cone :: Lambda
cone = lam ["s", "z"] $ App (Var "s") (Var "z")

ctwo :: Lambda
ctwo = lam ["s", "z"] $ App (Var "s") (App (Var "s") (Var "z"))


-- |Generalized numeral: convert a given integer >= 0 to a Church numeral
toNumeral :: Integer -> Lambda
toNumeral n | n >= 0= lam ["s", "z"] body
  where
    body = iterateN n (App (Var "s")) (Var "z")
    iterateN n f x | n <= 0 = x
                   | otherwise = iterateN (n - 1) f (f x)

-- |Convert a normalized numeral into an integer. Return Nothing if the 
-- numeral cannot be converted.
fromNumeral :: Lambda -> Maybe Integer
fromNumeral (Lam s (Lam z e))  = countApps e
  where
    countApps (Var z') | z' == z = Just 0
    countApps (App (Var s') e) | s' == s = (1 +) <$> countApps e
    countApps _ = Nothing
fromNumeral _ = Nothing

test_numerals = do
    test "fromNumeral (toNumeral 0)"
        (fromNumeral (toNumeral 0))
        (Just 0)
    test "fromNumeral (toNumeral 1)"
        (fromNumeral (toNumeral 1))
        (Just 1)
    test "fromNumeral (toNumeral 1024)"
        (fromNumeral (toNumeral 1024))
        (Just 1024)
    test "toNumeral 0"
        (toNumeral 0)
        czero
    test "toNumeral 1"
        (toNumeral 1)
        cone
    test "toNumeral 2"
        (toNumeral 2)
        ctwo
    test "toNumeral 3"
        (toNumeral 3)
        (lam ["s", "z"] $ App (Var "s") $ App (Var "s") $ App (Var "s") (Var "z"))
    test "fromNumeral ~0"
        (fromNumeral $ lam ["foo", "bar"] $ Var "bar")
        (Just 0)
    test "fromNumeral ~1"
        (fromNumeral $ lam ["foo", "bar"] $ App (Var "foo") (Var "bar"))
        (Just 1)
    test "fromNumeral ~16"
        (fromNumeral $ lam ["na", "batman"] $ 
            foldr App (Var "batman") $ replicate 16 $ Var "na")
        (Just 16)
    test "fromNumeral (λs s. s)"
        (fromNumeral $ lam ["s", "s"] $ Var "s")
        (Just 0)
    test "fromNumeral (λs z. z s)"
        (fromNumeral $ lam ["s", "z"] $ App (Var "z") (Var "s"))
        Nothing
    test "fromNumeral (λs z. s s)"
        (fromNumeral $ lam ["s", "z"] $ App (Var "s") (Var "s"))
        Nothing
    test "fromNumeral (λx y. z)"
        (fromNumeral $ lam ["x", "y"] $ Var "z")
        Nothing
    test "fromNumeral (λx. x)"
        (fromNumeral $ lam ["x"] $ Var "x")
        Nothing




-- |Successor of a natural number
csucc :: Lambda
csucc = Lam "n" (Lam "s" (Lam "z" 
        (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))

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

-- |Addition on naturals
cplus :: Lambda
cplus = lam ["m", "n"] $ Var "m" `App` csucc `App` Var "n"

-- some examples of cplus 
-- notation: I use ~n for Church numeral corresponding to n
test_cplus = do
    test "(cplus ~3 ~4) ~> 7"
        (fromNumeral $ normalize $ 
            App (App cplus (toNumeral 3)) (toNumeral 4))
        (Just 7)

-- |Subtraction on naturals
cminus :: Lambda
cminus = lam ["m", "n"] $ Var "n" `App` cpred `App` Var "m" 

test_cminus = 
  test "(cminus ~7 ~3) ~> 4" 
    (fromNumeral $ normalize $ app [cminus, toNumeral 7, toNumeral 3])
    (Just 4)

-- |Multiplication on naturals
ctimes :: Lambda
ctimes = lam ["m", "n"] $ Var "m" `App` (cplus `App` Var "n") `App` czero

test_ctimes = 
  test "(ctimes ~3 ~13) ~> 39" 
    (fromNumeral $ normalize $ app [ctimes, toNumeral 3, toNumeral 13])
    (Just 39)


-- |Boolean and
cand :: Lambda
cand = lam ["x", "y"] $ 
    Var "x" `App` Var "y" `App` (lam ["t", "f"] $ Var "f")

test_cand = do
    test' True True
    test' True False
    test' False True
    test' False False
  where
    test' a b = 
      test ("(and ~" ++ show a ++ " ~" ++ show b ++")")
          (fromChurchBool $ normalize $ app [cand, toChurchBool a, toChurchBool b])
          (Just $ a && b)
    

-- |Boolean or
cor :: Lambda 
cor = lam ["x", "y"] $ 
    Var "x" `App` (lam ["t", "f"] $ Var "t") `App` Var "y"

test_cor = do
    test' True True
    test' True False
    test' False True
    test' False False
  where
    test' a b = 
      test ("(or ~" ++ show a ++ " ~" ++ show b ++")")
          (fromChurchBool $ normalize $ app [cor, toChurchBool a, toChurchBool b])
          (Just $ a || b)
 
-- |Boolean negation
cnot :: Lambda
cnot = Lam "x" $ 
    Var "x" `App` (lam ["t", "f"] $ Var "f") `App` (lam ["t", "f"] $ Var "t")

test_cnot = do
    test "not ~True"
        (fromChurchBool $ normalize $ app [cnot, toChurchBool True])
        (Just False)
    test "not ~False"
        (fromChurchBool $ normalize $ app [cnot, toChurchBool False])
        (Just True)


-- |Conditional expression
cifthen :: Lambda
cifthen = lam ["b", "t", "f"] $ Var "b" `App` Var "t" `App` Var "f"

test_cifthen = do
    test "(cifthen ~true ~1 ~2)"
      (fromNumeral $ normalize $ app [cifthen, toChurchBool True, cone, ctwo])
      (Just 1)
    test "(cifthen ~false ~1 ~2)"
      (fromNumeral $ normalize $ app [cifthen, toChurchBool False, cone, ctwo])
      (Just 2)

-- |Is a given numeral zero?
ciszero :: Lambda
ciszero = Lam "n" $ 
  Var "n" `App` (lam ["x", "t", "f"] $ Var "f") `App` (lam ["t", "f"] $ Var "t")

test_ciszero = do
    test "(iszero ~0)"
        (fromChurchBool $ normalize $ app [ciszero, czero])
        (Just True)
    test "(iszero ~2)"
        (fromChurchBool $ normalize $ app [ciszero, ctwo])
        (Just False)

-- |"less or equal"
cleq :: Lambda 
cleq = lam ["m", "n"] $ ciszero `App` (cminus `App` Var "m" `App` Var "n")

test_leq = do
    test "(cleq ~0 ~2)"
        (fromChurchBool $ normalize $ app [cleq, czero, ctwo])
        (Just True)
    test "(cleq ~2 ~0)"
        (fromChurchBool $ normalize $ app [cleq, ctwo, czero])
        (Just False)
    test "(cleq ~2 ~2)"
        (fromChurchBool $ normalize $ app [cleq, ctwo, ctwo])
        (Just True)

-- |"less than"
clt :: Lambda 
clt = lam ["m", "n"] $
    cand `App` (cleq `App` Var "m" `App` Var "n") 
         `App` (cnot `App` (ceq `App` Var "m" `App` Var "n"))

test_clt = do
    test "(clt ~0 ~2)"
        (fromChurchBool $ normalize $ app [clt, czero, ctwo])
        (Just True)
    test "(clt ~2 ~0)"
        (fromChurchBool $ normalize $ app [clt, ctwo, czero])
        (Just False)
    test "(clt ~2 ~2)"
        (fromChurchBool $ normalize $ app [clt, ctwo, ctwo])
        (Just False)


-- |"greater than"
cgt :: Lambda
cgt = lam ["m", "n"] $ cnot `App` (cleq `App` Var "m" `App` Var "n")

test_cgt = do
    test "(cgt ~0 ~2)"
        (fromChurchBool $ normalize $ app [cgt, czero, ctwo])
        (Just False)
    test "(cgt ~2 ~0)"
        (fromChurchBool $ normalize $ app [cgt, ctwo, czero])
        (Just True)
    test "(cgt ~2 ~2)"
        (fromChurchBool $ normalize $ app [cgt, ctwo, ctwo])
        (Just False)


-- |"equal" for natural numbers
ceq :: Lambda
ceq = lam ["m", "n"] $
    cand `App` (cleq `App` Var "m" `App` Var "n")
         `App` (cleq `App` Var "n" `App` Var "m")

test_ceq = do
    test "(ceq ~0 ~2)"
        (fromChurchBool $ normalize $ app [ceq, czero, ctwo])
        (Just False)
    test "(ceq ~2 ~0)"
        (fromChurchBool $ normalize $ app [ceq, ctwo, czero])
        (Just False)
    test "(ceq ~2 ~2)"
        (fromChurchBool $ normalize $ app [ceq, ctwo, ctwo])
        (Just True)


-- |Pair constructor
cpair :: Lambda
cpair = lam ["l", "r", "s"] $ Var "s" `App` Var "l" `App` Var "r"

test_cpair =
    test "(cpair ~0 ~1)"
        (normalize $ app [cpair, czero, cone])
        (lam ["s0"] $ 
            app [ Var "s0"
                , lam ["s", "z1"] $ Var "z1"
                , lam ["s", "z"] $ app [Var "s", Var "z"] 
                ])

-- |Left of a pair
cleft :: Lambda
cleft = Lam "p" $ Var "p" `App` lam ["l", "r"] (Var "l")

test_cleft = 
    test "(cleft (cpair ~2 ~1))"
        (fromNumeral $ normalize $ app [cleft, app [cpair, ctwo, cone]])
        (Just 2)

-- |Right of a pair
cright :: Lambda
cright = Lam "p" $ Var "p" `App` lam ["l", "r"] (Var "r")

test_cright = 
    test "(cright (cpair ~2 ~1))"
        (fromNumeral $ normalize $ app [cright, app [cpair, ctwo, cone]])
        (Just 1)

-- |Fixpoint combinator
cfix :: Lambda
cfix = Lam "f" $
           (Lam "x" $ Var "f" `App` (Var "x" `App` Var "x")) 
           `App` (Lam "x" $ Var "f" `App` (Var "x" `App` Var "x"))

test_cfix = 
    test "(cfix (λf x. ((ciszero x) x (f (pred x)))) 8)"
        (fromNumeral $ normalize $
            app [ cfix
                , lam ["f", "x"] $ 
                      app [ ciszero, Var "x"
                          , Var "x"
                          , app [ Var "f", app [cpred, Var "x"] ]
                          ]
                , toNumeral 8
                ])
        (Just 0)


allTests :: IO ()
allTests = do
    test_lam_app
    test_bools
    test_numerals
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

