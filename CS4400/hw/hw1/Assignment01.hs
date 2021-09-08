{- |
Module      :  Assignment01
Description :  Assignment 1 submission for CS 4400.
Copyright   :  (c) Kevin Zhang

Maintainer  :  zhang.kevi@northeastern.edu
-}

module Assignment1 where

-- Checks to see if a natural number is odd.
isOdd :: Integer -> Bool
isOdd 0 = False
isOdd 1 = True
isOdd n = not (isOdd (n - 1))

-- Converts False to 0 and True to 1
boolToInteger :: Bool -> Integer
boolToInteger True = 1
boolToInteger False = 0

-- Converts a binary number (represented as a revered list of Bools) to an Integer.
binToInteger :: [Bool] -> Integer
binToInteger [] = 0
binToInteger [True] = 1
binToInteger [False] = 0
binToInteger (b : bs) = (boolToInteger b) + 2 * (binToInteger bs)

-- Determines if the list of Integers is sorted in descending order
isDescending :: [Integer] -> Bool
isDescending [] = True
isDescending (n : []) = True
isDescending (n1 : n2 : ns) = n1 >= n2 && isDescending (n2:ns)

data BinaryTree = Empty
                | Node Integer BinaryTree BinaryTree

-- Finds the sum of the elements in the BinaryTree
sumTree :: BinaryTree -> Integer
sumTree Empty = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

