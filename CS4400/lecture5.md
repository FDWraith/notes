## Lecture 5

Variables

```haskell
data SAE = Number Integer
		 | Add SAE SAE
		 | Sub SAE SAE
		 | Div SAE SAE
		 | Mul SAE SAE
		 | Let Variable SAE SAE -- for variable binding
		 | Var Variable         -- for variable evaluating
		 deriving (Eq, Show)
		 
-- example
ex_sae = Let "y" (Sub (Number 20) (Number 8))
			(Let "x" (Add (Var "y") (Number 4))
				(Add (Var "x") (Number 1)))
				
-- evals the SAE
eval :: SAE -> Integer
eval (Number n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Let x e1 e2) = eval (subst x (eval e1) e2)
eval (Var x) = error "Unexpected Variable With No Binding"


subst :: Variable -> Integer -> SAE -> SAE
subst x v (Number n) = Number n
subst x v (Var y) | x == y = Number v
				  | otherwise = (Var y)
subst x v (Add e1 e2) = Add (subst x v e1) (subst x v e2)
subst x v (Sub e1 e2) = Sub (subst x v e1) (subst x v e2)
subst x v (Mul e1 e2) = Mul (subst x v e1) (subst x v e2)
subst x v (Div e1 e2) = Div (subst x v e1) (subst x v e2)
subst x v (Let y e1 e2) 
	| x == y = Let y (subst x v e1) e2
	| otherwise Let y (subst x v e1) (subst x v e2)
```

Total Function -- a function that is defined for all inputs (has a valid output)

Partial Function -- a function that is not total

```haskell
-- Maybe type
{-
data Maybe a = Just a
			 | Nothing
-}

-- evals the SAE
eval :: SAE -> Maybe Integer
eval (Number n) = n
-- version 1
eval (Add e1 e2) =
	case eval e1 of
		Just n1 -> 
			case eval e2 of 
				Just n2 -> Just (n1 + n2)
				Nothing -> Nothing
		Nothing -> Nothing	

eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Let x e1 e2) = eval (subst x (eval e1) e2)
eval (Var x) = Nothing -- error "Unexpected variable"
```

