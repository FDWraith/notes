## Lecture 8

Lexical Scoping of functions

```haskell
eval g m (Call f e) =
	case get g f of
		Just (Defun _ x body) ->
			case eval g m e of 
				-- passes an empty Env into the function, so that any undefined
				-- variables are scoped properly inside of the function.
				Just v -> eval g (set empty x v) body
		Nothing -> Nothing
```

Defining functions as a value

```haskell
eval (App e1 e2) =
	case eval e1 of
		Just (FunVal x e) ->
			case eval e2 of 
				Just v2 -> 
eval (Fun x e) = Just (FunVal x e)
```

```haskell
apply :: (a -> a -> Maybe a) -> (Maybe a) -> (Maybe a) -> Maybe a
apply f Nothing _ = Nothing
apply f _ Nothing = Nothing
apply f (Just v1) (Just v2) f v1 v2

applyArith :: (Integer => Integer -> Integer) -> Value -> Value -> Maybe Value
applyArith 
```

