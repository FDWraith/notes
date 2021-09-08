module Lec05 where

import SimpleTests (test, testError, testErrorMsg)

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


{-

Partial functions: either we have a result or we don't

data Maybe a = Just a
             | Nothing
-}

{- partial functions with error messages

errors with 
data SuccessOrFailure a = Success a
                        | Failure String

a datatype like this is a special case of what's already provided:

data Either a b = Right b
                | Left a 
-}

-- |Evaluates the given SAE, computing the corresponding integer.
eval :: SAE -> Maybe Integer
eval (Number n) = Just n
eval (Add e1 e2) = 
  case eval e1 of
       Just n1 -> 
           case eval e2 of
                Just n2 -> Just (n1 + n2)
                Nothing -> Nothing
       Nothing -> Nothing
--eval (Sub e1 e2) = eval e1 - eval e2
--eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = 
    case eval e1 of
         Just n1 -> 
             case eval e2 of
                  Just 0 -> Nothing
                  Just n2 -> Just (n1 `div` n2)
                  Nothing -> Nothing
         Nothing -> Nothing
eval (Let x e1 e2) = 
    case eval e1 of
         Just n1 -> eval (subst x n1 e2)
         Nothing -> Nothing
eval (Var x) = Nothing -- error "Unexpected variable"


-- |We can make safe versions of functions that can crash a program
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x



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
                                                           --   ... if x /= y



-- f (List [Symbol "x", List [Symbol "foo", e1], e2])


{-
(let (x ...) (let (x (+ x 1)) (* x y)))

let x = 40 in let x = 50 in x + 5


    (let (x 40)


      (let (x (+ x 50)) 
        (+ x 5)))


3 + x - y = 3 + 32 - y
 = 3 + 32 - 45


  where x = 32 and y = 33 + 12 = 45
-}

{-
test_eval = do
  test "ex_sae1" (eval ex_sae1) 0
  test "ex_sae2" (eval ex_sae2) 4
  test "ex_sae3" (eval ex_sae3) 17

  -}
