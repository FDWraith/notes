## Lecture 1 : ACL2s Basics

Godel's incompleteness theorems

ACL2s allowed syntax

```
if
first
rest
cons
<
*
+
numerator
denominator
endp // empty?
listp
rationp
natp
equal
unary--
unary-/
consp
check=
```

```scheme
; f2l: List Nat -> List
; (f2l l n) rotates a list to the left n times
; where a rotation is tating the first element and
; putting it last
(define (f2l l n)
  )
```

Len in ACL2s

```
; len: List -> Nat
; (len l) takes a list l and returns the number of elements
(defunc len (l)
	input-contract(listp l)
	output-contract(natp (len l))
	(if (endp l)
		0
		(+ 1 (len (rest l)))))
		
(check= (len (list 1 2 3)) 3)
(check= (len nil) 0)
(check= (len (list (list 2 3) 'a)) 2)
```

ACL2s doesn't accept functions without any guaranteed contracts

* need to have `input-contract` and `output-contract` to define invariants

