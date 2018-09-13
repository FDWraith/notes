## Lecture 4: Numbers, Lists, and Symbols

```
(defunc div (x y)
	: input-contract (and (rationalp x) (and (rationalp y) (not (equal 0 y))))
	: output-contract (rationalp (div x y))
	(* x (unary-/ y)))
```

```unary-/``` takes one parameter and produces the reciprocal of the parameter

-  eg. ```(unary-/ 9)``` produces ```1/9```
- ```(unary-- 20)``` does something similar, by producing ```-20```

```quote``` takes values as it is (evaluates it to itself)

- eg. ```'(+ 3 4)``` produces ```(list '+ 3 4)```
- useful for making lists, making symbols

```
(defunc endp (l)
	: input-contract (listp l)
	: output-contract (booleanp (endp l)))
	(not (consp l)))

(defunc atom (l)
	: input-contract t
	: output-contract (booleanp (endp l)))
	(not (consp l)))
```

```
atom : All -> Boolean
endp : List -> Boolean
```

```
(defunc listp (a)
	: input-contract t
	: output-contract (booleanp (listp a))
	(if (consp a) 
		(listp a)
		(equal a nil)))
```

```and``` gets better

```
(and x (and y z)) ; should really be:
(and x y z) ; which is defined as a macro that becomes:
(if x (if y z nil) nil) 
```

```or``` also gets better

```
(or x y z) ; is really
(if x t (if y t z)) 
```

```cond``` looks like this

```
(cond (c1 e1)
	  (c2 e2)
	  (c3 e3)
	  ...
	  (t  en)) 
	 
; under the hood
(if c1 e1 (if c2 e2) (if c3 e3) ... (if cn-1 en-1 t))
```

