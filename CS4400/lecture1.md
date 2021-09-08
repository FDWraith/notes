## CS 4400: Programming Languages

What class is about

- language features from implementation-based perspective

**Programming Languages** - notation to express instructions to computer

- designed with humans in mind

Turing Tarpet -- minimum number of constructs to be Turing Complete.

### Semantics

- defines meaning of constructs in programming language
- various styles
  - operational - express operations to describe how PL behaves
  - denotational - translating PL into pure math
  - axiomatic - by means of properties defined by language constructs
- semantics = interpreters
  - implemented in Haskell (class)
  - our semantics are executable

### Haskell

```haskell
if True then "Yes" else "No"

-- one-line comment
{-
multi-line comment
-}

-- and, or, negation
--  &&  ||   not 

-- function definition
add3 x = x + 3

-- function application
add3 5


-- example
fact 0 = 1
fact n = n * fact (n - 1)

fact' n = if n > 0 then n * fact' (n - 1) else 1

fact'' n | n <= 0 = 1
		 | otherwise = n * fact'' (n - 1)
		 
[] -- empty list
1 : l -- "cons"
[1, 2, 3] = 1 (2 : (3 : []))

length' [] = 0
length' (x : xs) = 1 + length' xs

data Tree = Empty 
		  | Node Tree Int Tree
```

