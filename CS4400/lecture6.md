## Lecture 6

Equational Reasoning in Haskell

```haskell
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- can obey mathematical laws
eval (Add (Add e1 e2) e3)
=
eval (Add e1 e2) + eval e3
=
eval (eval e1 + eval e2) + eval e3
=
eval e1 + (eval e2 + eval e3)
=
eval (Add e1 (Add e2 e3))
```

Haskell Pure Expressions

```haskell
g :: Integer -> IO Integer
g x = do
	putStrLn (show x)
	return (x + x)
do 
	y <- g x
	return (2 * y)
	
-- IO is the outside world and implies a side effect
```

Referential transparency -- replace references with each other (without seeing what they do) and expect the same behavior. 

- Contrast with Java or other languages with side-effects.

Haskell Maps

```haskell
data Map k v == [(k, v)]

type Env = Map Variable Integer

empty :: Env
empty = []

get :: Env -> Variable -> Maybe Integer
get ((y, v) : m) x | x == y = Just v 
				   | otherwise = get m x

set :: Env -> Variable -> Integer -> Env
set m x v = (x, v) : m

get (set x v m) x = Just v
get (set x v m) y = get m y  -- if x /= y
get empty x = Nothing
```



