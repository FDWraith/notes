## Lecture 10

```haskell
type Variable = String

data Lambda = Var Variable
            | App Lambda Lambda
            | Lam Variable Lambda
            
beta :: Lambda -> Maybe Lambda
beta (Var x) = Nothing
beta (App (Lam x e1) e2) = Just (subst x e2 e1)
beta (App _ _) = Nothing
beta (Lam _ _) = Nothing

subst :: Variable -> Lambda -> Lambda -> Lambda
subst x e (Var y) | x == y = e
                  | otherwise = Var y
subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
subst x e (Lam y e') | x /= y && not (free y e) = 
						let z = fresh y (freeVars e `union` freeVars e')
					   	in Lam z (subst x e (subst y (Var z e')))
           			 | otherwise = Lam y e'


fresh :: Variable -> [Variable] -> Variable

           			 
free :: Variable -> Lambda -> Bool
free x (Var y) = y == x 
free x (App e1 e2) = free x e1 || free x e2
free x (Lam y e) | x == y = False
                 | otherwise = free x e
                 
freeVars :: Lambda -> [Variable]
freeVars (Var x) = [x]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Lam x e) = remove x (freeVars e)
```



```
Normal reduction -- always reduce the leftmost outermost redex
CBN reduction -- leftmost outermost, but not under lambdas
CBV reduction -- leftmost outermost, for any redex, reduce right-hand side fully
              -- no reduction under lambdas
```

#### Normal Form

```haskell
normal :: Lambda -> Maybe Lambda
normal (Var _) = Nothing
normal (App (Lam x e1) e2) = beta (App (Lam x e1) e2)
normal (App e1 e2) =
	case normal e1 of 
		Just e1' -> Just (App e1' e2)
		Nothing -> 
			case normal e2 of
				Just e2' -> Just (App e1 e2')
				Nothing -> Nothing
normal (Lam x e) = do
	e' <- normal e
	return (Lam x e')
	
-- normal form (multi-step normal reduction)
normalForm :: Lambda -> Lambda
normalForm e = 
	case normal e of
		Just e' -> normalForm e'
		Nothing -> Nothing
```

#### CBN (Call By Name) Reduction

```haskell
cbn :: Lambda -> Maybe Lambda
cbn (Var _) = Nothing
cbn e@(App (Lam _ _) ) = beta e
cbn (App e1 e2) = 
	case cbn e1 of
		Just e1' -> Just (App e1' e2)
		Nothing ->
			case cbn e2 of
				Just e2' -> Just (App e1 e2')
				Nothing -> Nothing
cbn (Lam x e) = Nothing

-- do the same thing with normal form, time to abstract
cbnForm :: Lambda -> Lambda
cbnForm e =
	case cbn e of
		Just e' -> cbnForm e'
		Nothing -> Nothing
```

#### Iterate (abstraction of `cbnForm` and `normalForm`)

```haskell
iterate' :: (a -> Maybe a) -> a -> a
iterate' f x = 
	case f x of 
		Just x' -> iterate' f x'
		Nothing -> x

cbnForm = iterate' cbn
```

#### CBV (Call By Value) Reductions

```haskell
cbv :: Lambda -> Maybe Lambda
cbv (Var _) = Nothing
cbv (App (Lam x e1) e2) =
	case cbv e2 of
		Just e2' -> Just (App (Lam x e1) e2')
		Nothing -> beta (App (Lam x e1) e2)
cbv (App e1 e2) =
	case cbv e1 of 
		Just e1' -> Just (App e1' e2)
        Nothing -> 
        	case cbv e2 of 
        		Just e2' -> Just (App e1 e2')
        		Nothing -> Nothing
cbv (Lam _ _) = Nothing
```

#### Omega

$$
(\lambda x. x \hspace{3pt} x) (\lambda x. x \hspace{3pt} x)
$$



