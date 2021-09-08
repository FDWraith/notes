## Lecture 7

Defining Programs and Global Variables

```haskell
{-
<SAE> ::= <Number>
		| (+ <SAE> <SAE>)
		| (- <SAE> <SAE>)
		| (/ <SAE> <SAE>)
		| (* <SAE> <SAE>)
		| (let (<Variable> <SAE>) <SAE>)
		| <Variable>


<Program> ::= <SAE>
		    | (defun <Variable> (<Variable>) <SAE>) <Program>
		    
or 

<GlobalDef> ::= (defun <Variable> (<Variable>) <SAE>)
<Program> ::= <GlobalDef>* <SAE>
		
-}

type Variable = String

data SAE = Number Integer
	     | Add SAE SAE
	     | Sub SAE SAE
	     | Mul SAE SAE
	     | Div SAE SAE
	     | Let Variable SAE SAE
	     | Var Variable
	     deriving (Eq, Show)
	     
data Program = Program [GlobalDef] SAE

newtype GlobalDef = Defun Variable Variable SAE

-- note: newtype same as data, but only with a single constructor
--       compiles more efficiently

type Env = Map Variable Integer

type GlobalEnv = Map Variable GlobalDef

runProgram :: Program -> Maybe Integer
runProgram (Program globalDefs e) = eval globals empty e
	where
		globals = collectDefs globalDefs
		collectDefs [] = empty
		collectDefs (Defun f x body : globalDefs) = 
			set (collectDefs globalDefs) f (Defun f x body)
```

