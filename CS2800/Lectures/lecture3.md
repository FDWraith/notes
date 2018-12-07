##  Lecture 3: Full-Blown ACL2s

Not all lists are cons, and not all cons are lists

- ```(cons 4 4)``` is valid, but is not a list
- ```nil``` is the empty list, but is not a cons

Expression is anything that can be meaningfully evaluated

- [[4]]
- [[ (len '(1 2 3 4)) ]]

equal: All x All -> "Boolean"

- evaluates both expressions before comparison

```
(defunc booleanp (x)
	: input-contract t
	: output-contract (booleanp (booleanp x))
	(if (equal x t)
		t
		(equal x nil)))

(defunc not (x)
	: input-contract (booleanp x)
	: output-contract (booleanp (not x))
	(if x nil t))
		
(defunc and (x y)
	: input-contract (if (booleanp x) (booleanp y) nil)
	: output-contract (booleanp (and x y))
	(if x y nil))
	
(defunc implies (x y)
	: input-contract (and (booleanp x) (booleanp y))
	: output-contract (booleanp (implies x y))
	(if x y t))
```

Barebones mode operations:

```
numbers = rationals
* 
+
<
numerator
denominator
unary--
unary-/
```

Numbers are stored as 
$$
\frac {a_{integer}}{b_{positive integer}}
$$




