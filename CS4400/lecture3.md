## Lecture 3

```haskell
{-
<SAE> ::= <Number>
		| <SAE> + <SAE>
		| <SAE> - <SAE>
		| <SAE> * <SAE>
-}

data SAE = Number Integer
		 | Add SAE SAE
		 | Sub SAE SAE
		 | Mul SAE SAE
		 | Div SAE SAE
		 deriving (Eq, Show) -- uses the default equality and print
		 
-- Examples
sae1, sae3 :: SAE
sae1 = (Sub (Add (Number 1) (Number 3)) (Number 3))
sae2 = (Div (Number 44) (Add (Mul (Number 2) (Number 5)) (Number 1)))
```

S-expressions : convenient way of expressing language

Compiled Language: Program -> Compiler -> Input + Binary Executable_Runtime -> Output

- adv: fast program
- dis: have to compile based on platform that is on

Interpreted Language: Program + Input -> Interpreter_Runtime -> Output

- adv: ease of use

```haskell
eval :: SAE -> Integer
eval (Number n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = div (eval e1) (eval e2)

-- expectations 
test1, test2 :: Bool
test1 = eval sae1 == 1
test2 = eval sae2 == 4

-- prefix notation for operatiors
(+) (eval e1) (eval 2)
-- infix notation for functions
(eval e1) `div` (eval e2)

```

Tuples

```haskell
(12, "Hello", 32.5) -- fixed length "list" 

first :: (Integer, String, Float) -> Integer
first (x, _, _) = x
```

Lists

```haskell
length :: [a] -> Integer
length [] = 0
length (_ : xs) = 1 + length xs

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [_] = []
everyOther (_ : y : xs) = y : everyOther xs
```

Types

```haskell
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x) : (map f xs)
```

Classes in Haskell behave like interfaces -- promises of necessary functions and behavior

Tools in hci

```
:r -- reloads moduel
:t <function_name> -- shows type information of function
:i <class_name> -- shows information of a class
```

