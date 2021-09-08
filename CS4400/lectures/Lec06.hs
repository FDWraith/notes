module Lec06 where

import SimpleTests (test, testError, testErrorMsg)

import Prelude hiding (get)

-- <SAE> ::= <Number>
--         | (+ <SAE> <SAE>)
--         | (- <SAE> <SAE>)
--         | (* <SAE> <SAE>)
--         | (/ <SAE> <SAE>)
--         | (let (<Variable> <SAE>) <SAE>)
--         | <Variable>

-- (let (x (+ 4 2)) (* x x))

-- (+ x x)
-- (let (x (+ y 4)) (+ x 1))
-- (let (y (* 2 20)) (let (x (+ y 4)) ...))

type Variable = String

-- |Abstract syntax tree for SAE
data SAE = Number Integer
         | Add SAE SAE
         | Sub SAE SAE
         | Mul SAE SAE
         | Div SAE SAE
         | Let Variable SAE SAE
         | Var Variable
         deriving (Eq, Show)

-- Examples
ex_sae1, ex_sae2 :: SAE
-- (- (+ 1 2) 3)
ex_sae1 = Sub (Add (Number 1) (Number 2)) (Number 3)
-- (/ 44 (+ (* 2 5) 1))
ex_sae2 = Div (Number 44) (Add (Mul (Number 2) (Number 5)) (Number 1))
-- (let (y (- 20 8)) (let (x (+ y 4)) (+ x 1)))
ex_sae3 = Let "y" (Sub (Number 20) (Number 8)) 
            (Let "x" (Add (Var "y") (Number 4)) 
              (Add (Var "x") (Number 1)))

type Map k v = [(k, v)]

type Env = Map Variable Integer

empty :: Env
empty = []

get :: Env -> Variable -> Maybe Integer
get ((y, v) : m) x | x == y = Just v
                   | otherwise = get m x
get [] x = Nothing

set :: Env -> Variable -> Integer -> Env
set m x v = (x, v) : m

--get ((x, v) : m) x = Just v
--get ((x, v) : m) y = get m y  -- if x /= y
--get [] x = Nothing

-- |Evaluates the given SAE, computing the corresponding integer.
eval :: Env -> SAE -> Maybe Integer
eval m (Number n) = Just n
eval m (Add e1 e2) = 
    case eval m e1 of
         Just n1 -> 
             case eval m e2 of
                  Just n2 -> Just (n1 + n2)
                  Nothing -> Nothing
         Nothing -> Nothing
eval m (Sub e1 e2) = 
    case eval m e1 of
         Just n1 -> 
             case eval m e2 of
                  Just n2 -> Just (n1 - n2)
                  Nothing -> Nothing
         Nothing -> Nothing
eval m (Mul e1 e2) = 
    case eval m e1 of
         Just n1 -> 
             case eval m e2 of
                  Just n2 -> Just (n1 * n2)
                  Nothing -> Nothing
         Nothing -> Nothing
eval m (Div e1 e2) = 
    case eval m e1 of
         Just n1 -> 
             case eval m e2 of
                  Just 0 -> Nothing
                  Just n2 -> Just (n1 `div` n2)
                  Nothing -> Nothing
         Nothing -> Nothing
eval m (Let x e1 e2) = 
    case eval m e1 of
         Just n1 -> eval (set m x n1) e2
         Nothing -> Nothing
eval m (Var x) = get m x -- error "Unexpected variable"


{-
-- |substitution: replace any matching variable with the given value
subst :: Variable -> Integer -> SAE -> SAE                 -- in "formal notation":
subst x v (Number n) = Number n                            -- n[x := v] = n
subst x v (Var y) | x == y = Number v                      -- x[x := v] = v
                  | otherwise = Var y                      -- y[x := v] = y if x /= y
subst x v (Add e1 e2) = Add (subst x v e1) (subst x v e2)  -- (? e1 e2)[x := v]
subst x v (Sub e1 e2) = Sub (subst x v e1) (subst x v e2)  --   = (? e1[x := v] e2[x:= v))
subst x v (Mul e1 e2) = Mul (subst x v e1) (subst x v e2)  --   ... for ? in {+, -, * /}
subst x v (Div e1 e2) = Div (subst x v e1) (subst x v e2)
subst x v (Let y e1 e2)                                    -- (let (x e1) e2)[x := v] 
    | y == x = Let y (subst x v e1) e2                     --   = (let (x e1) e1[x := v])
    | otherwise = Let y (subst x v e1) (subst x v e2)      -- (let (y e1) e2)[x := v]
                                                           --   = (let (y e1[x := v]) e2[x := v])
  -}                                                         --   ... if x /= y


test_eval = do
  test "ex_sae1" (eval empty ex_sae1) (Just 0)
  test "ex_sae2" (eval empty ex_sae2) (Just 4)
  test "ex_sae3" (eval empty ex_sae3) (Just 17)

