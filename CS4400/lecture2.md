## Lecture 2

Every programming language is really a collection of strings or possible programs

Syntax -- a collection of rules to ensure valid programs

#### Concrete Syntax Aspects

- Lexical Details
  - keywords
  - operations
  - expressions
- Structure
  - usually established using context-free grammars
  - details of how the programming language looks / is formatted

#### Abstract Syntax

- easier to reason about 
- data-type that defines languages without being concerned with concrete rules



Simple Arithmetic Expression (SAE)

- Ex. 1 + 2 / 3, 31 - 4 + 2 * 8

Variant 1 :

- if n is a number, then is a SAE
- If $$e_1$$ and $$e_2$$ are SAE, then so is
  - $$e_1 + e_2$$
  - $$e_1 - e_2$$
  - $$e_1 * e_2$$
  - $$e_1 \div e_2$$

Using Recursion to apply rules again to build out expressions

Another way to write this (more formally and with **productions**):

```
<SAE> ::= <Integer>
		| <SAE> + <SAE>
		| <SAE> - <SAE>
		| <SAE> * <SAE>
		| <SAE> / <SAE>
```

Proving that $$1 + 2 \space / \space 3$$ is a SAE

```
<SAE> => <SAE> / <SAE>
  	  => <SAE> + <SAE> / <SAE>
  	  => <Integer> + <Integer> / <SAE>
  	  => 1 + 2 / 3
```

Using Haskell (with PEMDAS)

```haskell
<Expression> ::= <Expression> + <Term>
			   | <Expression> - <Term>
			   | <Term>
<Term> ::= <Term> * <Term>
		 | <Term> / <Term>
		 | <Digit>
```



---

### Haskell Notes

There are two integer types (`Integer`) and (`Int`)

- `Int` is machine-bounded integer (standard 16-bit)
- `Integer` is unlimited-bounded integer
- Requires explicit conversion 

```haskell
-- Char type
'e' '4' '\n'
-- Bool type
True False
-- "String" type
['e', '4', '\n']

-- Type signatures
x :: Integer
x = 14

c :: Char
c = 'c'

name :: String
name = "Zaphod"

list :: [Integer]
list = [1, 2, 3]

list1 :: [[Bool]]
list1 = [[True], [False, True]]

-- Functions 

areaOfSquare :: Integer -> Integer
areaOfSquare x = x * x

-- takes two arguments
takes2 :: Integer -> Integer -> String
```

Defining new Types

```haskell
type Number = Integer
type String = [Char]

-- enumerations
data Color = Red | Green | Blue

-- right hand side are constructors
data NumberOrName = Number Integer
				  | Name String
				  
Number 42
Name "Zaphod"

data MyIntegerList = Nil
				   | Cons Integer MyIntegerList
				   
Cons 1 (Cons 2 Nil)

colorToString :: Color -> String
colorToString color
	case color of
		Red -> "red"
		Green -> "green"
		Blue -> "blue"
		
isName :: NumberOrName -> Bool
isName (Name _) = True
isName _ = False
```

Back to SAEs

```haskell
data SAE = Number Integer -- needs the constructor
		 | Add SAE SAE
		 | Sub SAE SAE
		 | Mul SAE SAE
		 | Div SAE SAE
```

