## Lecture 14: Definitional Principle

```lisp
(defunc f(x)
    :input-contract (natp x)
    :output-contract (natp (f x))
    (+ 1 (f x)))

T1) (natp x) => (natp (f x))           ; IC => OC
T2) (natp x) => (f x) = (+ 1 (f x))    ; Definitional Theoremm
T3) (natp x) => ((+ 1 x) =/= x)        

#|
   {T3 | ((x (f x)))}
 = (natp (f x)) => ((+ 1 (f x)) =/= (f x))
   {T1}
   (natp x) => ((+ 1 (f x) =/= (f x)))
 = {T2}
   (natp x) => ((+ 1 (f x) =/= (f x))) /\ ((+ 1 (f x) = (f x)))
 = {PL}
   (natp x) => false
|3
```

```f(x)``` is non-terminating, so any logic that derives from it is unsound

#### The Definitional Principle

```
The definition 
(defunc f (x1 ... xn)
	:input-contract IC
	:output-contract OC
	<Body of f>)
is admissible provided:
1. f is a new funcion symbol
2. the x1 ... xn are distinct
3. the body is a term (well-formed legal expression given current history) possibly using f recursively as a function symbol, and mentions no variables freely except x1 ... xn.
4. The function terminates
5. IC => OC
6. The body contracts hold under the assumption that the IC holds
   Thus IC => (f x1 ... xn) = <Body of f>
```



