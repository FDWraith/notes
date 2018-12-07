## Lecture 2: More ACL2s Basics

#### F2L and Append

```
; List Nat -> List
; (f2l l n) "rotates" the list l by n number of elements
; where each rotation pushes the first element of the list
; to after the last element. 
(defunc f2l (l n)
	: input-contract (and (listp l) (natp n))
	: output-contract (listp (f2l l n))
	(if (or (equal n 0) (endp l))
		l
		(f2l (app (rest l) (list (first l))) (+ n -1))))
	
(check= (f2l '(1 2 3 4 5) 2) '(3 4 5 1 2))
(check= (f2l nil 1) nil)
(check= (f2l '(1 2 3) 0) '(1 2 3))
(check= (f2l '(1 2 3) 3) '(1 2 3))

; List List -> List
; (app x y) appends list x and y
(defunc app (x y)
	: input-contract (and (listp x) (listp y))
	: output-contract (listp (app x y))
	(if (endp x) 
		y
		(cons (first x) (app (rest x) y))))
		
(check= (app nil nil) nil)
(check= (app nil '(1)) '(1))
(check= (app '(1) nil) '(1))
(check= (app '(1 2) '(3 4)) '(1 2 3 4))
(check= (app '(2 3) '(3 4 5 6)) '(2 3 3 4 5 6))
```

