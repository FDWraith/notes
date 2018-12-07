## Lecture 15: Admissible

Rules (Again):

1. f is unique
2. variables are from parameters
3. Body is well-formed
4. Terminates
5. IC => Body = (f x)
6. IC => OC

#### Prove app terminates

```lisp
(defunc app (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract (listp (app x y))
    (if (endp x) 
        y
        (cons (first x) (app (rest x) y))))

; Measure function for app (to check for termination)
(defunc m-app (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract (natp (m-app x y))
    (len x))
```

A measure function *m* is a function that must satisfy the following

1. *m* is admissible
2. *m* has the same input parameters as f
3. *m* has the same input contract as f
4. the output contract of *m* is (natp (m ...))
5. On every recursive call of f, *m* (applied to the arguments of the recursive call) **decreases** under the conditions that led to the recursive call.

```lisp
#|
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
-------------
C4. (listp (rest x)) {C3, C1, Def endp, def lisp}

   (m-app x y)
 = {Def x-app}
   (len x)
 = {Def of len, C3}
   (+ 1 (len (rest x)))
 > {Arithmetic}
   (len (rest x))
 = {Def of m-app}
   (m-app (rest x) y)

(m-app x y) > (m-app (rest x) y), so (app x y) terminates
|#
```

Another example:

```lisp
; Collatz Conjecture
(defunc c (n)
    :input-contract (natp n)
    :output-contract (natp (c n))
    (cond 
        ((< n 2) n)
        ((integerp (/ n 2)) (c (/ n 2)))
        (t (c (+ 1 (* 3 n))))))
```

