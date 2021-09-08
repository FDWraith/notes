{- |
Module      :  Assignment02
Description :  Assignment 2 submission for CS 4400.
Copyright   :  (c) Kevin Zhang

Maintainer  :  zhang.kevi@northeastern.edu
-}

module Assignment02 where

import SimpleTests

{-
BNF

<SExpr> ::= <Integer>
          | <String>
          | ()
          | (<NonEmptyList>)

<NonEmptyList> ::= <SExpr>
                 | <SExpr> <NonEmptyList>
-}

data SExpr = Number Integer
           | Symbol String
           | Array [SExpr]
           deriving (Eq, Show)

ex_sexpr1 = Number 2
ex_sexpr2 = Array [Symbol "h"]
ex_sexpr3 = Symbol "blah"
ex_sexpr4 = Symbol "+"
ex_sexpr5 = Array [ex_sexpr4, ex_sexpr1, ex_sexpr3]
ex_sexpr6 = Array []

-- converts an SExpr into a printable string.
showSExpr :: SExpr -> String
showSExpr (Number n) = show n
showSExpr (Symbol s) = s
showSExpr (Array []) = "()"
showSExpr (Array (x:xs)) = "(" ++ showSExpr x ++ (foldl showL "" xs) ++ ")"
  where showL s e = s ++ " " ++ showSExpr e

test_showSExpr_1 = test "showSExpr_1" (showSExpr ex_sexpr1) "2"
test_showSExpr_2 = test "showSExpr_2" (showSExpr ex_sexpr2) "(h)"
test_showSExpr_3 = test "showSExpr_3" (showSExpr ex_sexpr3) "blah"
test_showSExpr_4 = test "showSExpr_4" (showSExpr ex_sexpr4) "+"
test_showSExpr_5 = test "showSExpr_5" (showSExpr ex_sexpr5) "(+ 2 blah)"
test_showSExpr_6 = test "showSExpr_6" (showSExpr ex_sexpr6) "()"


data SAE = Number' Integer
         | Add SAE SAE
         | Sub SAE SAE
         | Mul SAE SAE
         | Div SAE SAE
         deriving (Eq, Show)

ex_sae_1 = Number' 1
ex_sae_2 = Add (Number' 2) (Number' 4)
ex_sae_3 = Mul ex_sae_1 ex_sae_2
ex_sae_4 = Sub (Number' 5) (Number' 4)
ex_sae_5 = Div ex_sae_3 ex_sae_4

-- as SExpressions
ex_sae_sexpr_1 = Number 1
ex_sae_sexpr_2 = Array [(Symbol "+"), (Number 2), (Number 4)]
ex_sae_sexpr_3 = Array [(Symbol "*"), ex_sae_sexpr_1, ex_sae_sexpr_2]
ex_sae_sexpr_4 = Array [(Symbol "-"), (Number 5), (Number 4)]
ex_sae_sexpr_5 = Array [(Symbol "/"), ex_sae_sexpr_3, ex_sae_sexpr_4]

-- converts an SExpr (with symbols restricted to +, -, *, /) into SAE
fromSExpr :: SExpr -> SAE
fromSExpr (Number n) = Number' n
fromSExpr (Array ( (Symbol s) : e1 : e2 : es)) = case (s) of
                         "+" -> Add (fromSExpr e1) (fromSExpr e2)
                         "-" -> Sub (fromSExpr e1) (fromSExpr e2)
                         "/" -> Div (fromSExpr e1) (fromSExpr e2)
                         "*" -> Mul (fromSExpr e1) (fromSExpr e2)
fromSExpr _ = Number' 0 -- Gibberish case; all expressions should be valid.

test_fromSExpr_1 = test "fromSExpr_1" (fromSExpr ex_sae_sexpr_1) ex_sae_1
test_fromSExpr_2 = test "fromSExpr_2" (fromSExpr ex_sae_sexpr_2) ex_sae_2
test_fromSExpr_3 = test "fromSExpr_3" (fromSExpr ex_sae_sexpr_3) ex_sae_3
test_fromSExpr_4 = test "fromSExpr_4" (fromSExpr ex_sae_sexpr_4) ex_sae_4
test_fromSExpr_5 = test "fromSExpr_5" (fromSExpr ex_sae_sexpr_5) ex_sae_5


-- converts an SAE into a SExpr
toSExpr :: SAE -> SExpr
toSExpr (Number' n) = Number n
toSExpr (Add s1 s2) = Array [(Symbol "+"), (toSExpr s1), (toSExpr s2)]
toSExpr (Mul s1 s2) = Array [(Symbol "*"), (toSExpr s1), (toSExpr s2)]
toSExpr (Div s1 s2) = Array [(Symbol "/"), (toSExpr s1), (toSExpr s2)]
toSExpr (Sub s1 s2) = Array [(Symbol "-"), (toSExpr s1), (toSExpr s2)]

test_toSExpr_1 = test "toSExpr_1" (toSExpr ex_sae_1) ex_sae_sexpr_1
test_toSExpr_2 = test "toSExpr_2" (toSExpr ex_sae_2) ex_sae_sexpr_2
test_toSExpr_3 = test "toSExpr_3" (toSExpr ex_sae_3) ex_sae_sexpr_3
test_toSExpr_4 = test "toSExpr_4" (toSExpr ex_sae_4) ex_sae_sexpr_4
test_toSExpr_5 = test "toSExpr_5" (toSExpr ex_sae_5) ex_sae_sexpr_5

data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Eq, Show)

ex_bintree_1 = Empty
ex_bintree_2 = (Node 5 Empty Empty)
ex_bintree_3 = (Node 20 (Node 10 (Node 5 Empty Empty) Empty) Empty)
ex_bintree_4 = (Node [20] Empty Empty)
ex_bintree_5 = (Node [20, 10, 5] (Node [15] Empty Empty) (Node [25, 35] Empty (Node [55] Empty Empty)))

-- applies the given function to every element present in BinaryTree, and returns resulting tree
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Empty = Empty
treeMap f (Node a lt rt) = Node (f a) (treeMap f lt) (treeMap f rt)

-- doubles a number
double :: Integer -> Integer
double x = x * 2

ex_bintree_3_expected = (Node 40 (Node 20 (Node 10 Empty Empty) Empty) Empty)
ex_bintree_4_expected = (Node 20 Empty Empty)
ex_bintree_5_expected = (Node 20 (Node 15 Empty Empty) (Node 25 Empty (Node 55 Empty Empty)))

test_treeMap_1 = test "treeMap_1" (treeMap double ex_bintree_1) Empty
test_treeMap_2 = test "treeMap_2" (treeMap double ex_bintree_2) (Node 10 Empty Empty)
test_treeMap_3 = test "treeMap_3" (treeMap double ex_bintree_3) ex_bintree_3_expected
test_treeMap_4 = test "treeMap_4" (treeMap head ex_bintree_4) ex_bintree_4_expected
test_treeMap_5 = test "treeMap_5" (treeMap head ex_bintree_5) ex_bintree_5_expected

-- applies the given function to the initial value the specified number of times
iterateN :: (a -> a) -> Integer -> a -> a
iterateN f 0 init = init
iterateN f n init = iterateN f (n - 1) (f init)


-- doubles a tree
doubleT :: BinaryTree Integer -> BinaryTree Integer
doubleT t = treeMap double t

test_iterateN_1 = test "iterateN_1" (iterateN double 5 0) 0
test_iterateN_2 = test "iterateN_2" (iterateN double 3 2) 16
test_iterateN_3 = test "iterateN_3" (iterateN doubleT 2 ex_bintree_2) ex_bintree_4_expected
test_iterateN_4 = test "iterateN_4" (iterateN tail 3 ["A", "B", "C"]) []

main :: IO ()
main = do
  -- showExpr tests
  test_showSExpr_1
  test_showSExpr_2
  test_showSExpr_3
  test_showSExpr_4
  test_showSExpr_5
  test_showSExpr_6

  -- fromSExpr tests
  test_fromSExpr_1
  test_fromSExpr_2
  test_fromSExpr_3
  test_fromSExpr_4
  test_fromSExpr_5

  -- toSExpr tests
  test_toSExpr_1
  test_toSExpr_2
  test_toSExpr_3
  test_toSExpr_4
  test_toSExpr_5

  -- treeMap tests
  test_treeMap_1
  test_treeMap_2
  test_treeMap_3
  test_treeMap_4
  test_treeMap_5

  -- iterateN tests
  test_iterateN_1
  test_iterateN_2
  test_iterateN_3
  test_iterateN_4
  
  
