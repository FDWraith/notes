export module Quiz2 where

data Expr = Add Expr Expr
          | Number Integer 
          | Let Variable Expr Expr
          | Var Variable

eval :: Expr -> Integer
eval (Add e1 e2) = eval e2 + eval e1
eval (Number n) = n
eval (Let x e1 e2) = eval (subst x (eval e1) e2)
eval (Var x) = error $ "Variable " ++ x ++ " not found"

subst :: Variable -> Integer -> Expr -> Expr
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (Number n') = Number n
subst x n (Let y e1 e2) | x == y = Let x (subst y n e1) e2
                        | otherwise = Let x (subst y n e1) (subst x n e2) 
subst x n (Var y) | x == y = Number n
                  | otherwise = Var x
