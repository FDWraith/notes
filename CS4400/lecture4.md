## Lecture 4

```haskell
{-
A JSON element is one of:
  - a number (integer)
  - a string
  - a boolean
  - a `null`
  - an array of 0 or more JSON elements (enclosed in [ ])
  - an object, containing 0 or more tag-element pairs (enclosed in { })

Example:
  {
    "class": "CS4400",
    "instructor": "Fer",
    "students": 48,
    "days": ["Tuesday", "Friday"]
  }
-}

{- BNF

<JSON> ::= <Integer>
	     | <String>
	     | <Boolean>
	     | null
	     | [ ]
	     | [ <NonEmptyList> ] -- can merge empty case with [ <NonEmptyList>? ]
	     | { }
	     | { <NonEmptyTaggedList> }

<NonEmptyList> ::= <JSON>
				 | <JSON> , <NonEmptyList>
				 
<NonEmptyTaggedList> ::= <String> : <JSON>
					   | <String> : <JSON> , <NonEmptyTaggedList>

-- other BNF extensions
<Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<Integer> ::= <Digit>+ -- 1 or more
<Integer> ::= <Digit> <Digit>* -- 0 or more

-}

data Json = Number Integer
		  | Str String
		  | Boolean Bool
		  | Null
		  | Array [Json]
		  | Object [(String, Json)]
-- matter of taste; can introduce actual object instead
data JsonObject = JsonObject String Json


-- equals is     ==
-- not equals is /=
```

Enumerable

```haskell
data Four = One | Two | Three | Four
	      deriving (Enum)
	      
-- makes this possible:
[One .. Four] -- Equal to [One, Two, Three, Four]

-- integers are enumerable
[1 .. 12] -- Equal to [1,2,3,4,5,6,7,8,9,10,11,12]
```

Higher Order Functions

```haskell
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ v [] = v
foldRight f v (x: xs) f x (foldRight f v (xs))

doubleAll :: [Integer] -> [Integer]
doubleAll l = 
	let double x = 2 * x
	in map double l
	
-- alternative
doubleAll :: [Integer] -> [Integer]
doubleAll l = map double l
	where double = 2 * x
```

IO

```haskell
main :: IO ()
main = do
	putStrLn "tell me your name"
	name <- getLine
	putStrLn ("Hello, " ++ name)
```

Testing Library

```haskell
import SimpleTests

f x = x
test_f = do
	test "Identity" (f 10) 10
	test "Identity 2" (f 2) 2

-- types beign passed into test need Eq, Show
main :: IO ()
main = do
	test_f
```

