### Lecture 13

```lisp
(defunc in (e x)
    :input-contract (listp x)
    :ouput-contract (booleanp (in e x))
    (if (endp x)
        nil
        (if (equal e (first x))
            t
            (in e (rest x)))))

(defunc del-all (e x)
    :input-contract (listp x)
    :output-contract (listp (del-all e x))
    (if (endp x)
        x
        (if (equal e (first x))
            (del-all e (rest x))
            (cons (first x) (del-all e (rest x))))))

#|
Case 1:
C1. (listp x)
C2. (endp x)
|#
  (not (in e (del-all e x)))
= {Def of del, C2, if axiom}
  (not (in e l))
= {Def of in, C2, if axiom}
  (not nil)
= {Negation}
  t

#|
Case 2:
C1. (listp x)
C2. ~(endp x)
C3. (e = (first x))
C4. [(listp (rest x)) => (not (in e (del-all e (rest x))))]
-------------
C5. (consp x) {C1, C2, Def endp}
C6. (listp (rest x)) {C5, C1}
C7. (not (in e (del-all e (rest x)))) {MP, C6, C4}
|#
   (not (in e (del-all e x)))
 = {Def of del, C2, if axiom}
   (not (in e (if (equal e (first x))
                  (del-all e (rest x))
                  (cons (first x) (del-all e (rest x))))))
 = {C3, if axiom}
   (not (in e (del-all e (rest x))))
 = {C7}
   t

#|
Case 3:
C1. (listp x)
C2. ~(endp x)
C3. ~(e = (first x))
C4. [(listp (rest x)) => (not (in e (del-all e (rest x))))]
------------
C5. (consp x) {C1, C2, Def endp}
C6. (listp (rest x)) {C5, C1}
C7. (not (in e (del-all e (rest x)))) {MP, C6, C4}
C8. ~(e = (first (cons (first x) (del-all e (rest x))))) {C3, first-rest axiom}
|#
    (not (in e (del-all e x)))
  = {Def of del, C2, if axiom}
	(not (in e (if (equal e (first x))
                  (del-all e (rest x))
                  (cons (first x) (del-all e (rest x))))))
  = {C3, if axiom}
    (not (in e (cons (first x) (del-all e (rest x)))))
  = {Def of in, consp axiom, Def of endp, C8}
    (not (in e (rest (cons (first x) (del-all e (rest x))))))
  = {first-rest axiom}
    (not (in e (del-all e (rest x))))
  = {C7}
     t
```

```lisp
(defunc sumn (n)
    :input-contract (natp n)
    :output-contract (natp (sumn n))
   	(if (equal n 0)
        0
        (+ n (sumn (- n 1)))))

#|
Prove: (natp n) => (sumn n) = (/ (* n (+ n 1)) 2)
Case 1. When n is zero
C1. (natp n)
C2. (n = 0)

LHS:(sumn n)
  = {Def of sumn, C2, if axiom}
    0
RHS:(/ (* n (+ n 1)) 2)
  = {Arithmethic, C2}
    0
LHS = RHS, so the conjecture is true

Case 2. When is not zero, assuming inductive hypthesis
C1. (natp n)
C2. ~(n = 0)
C3. (natp (- n 1)) => (sumn (- n 1)) = (/ (* (- n 1) n) 2)
-------------
C4. (natp (- n 1)) {C1, C2, Def nat}
C5. (sumn (- n 1)) = (/ (* (- n 1) n) 2) {C4, C3, MP}

LHS:(sumn n)
  = {Def of sumn, C2, if axiom}
    (+ n (sumn (- n 1)))
  = {C5}
    (+ n (/ (* (- n 1) n) 2)) 
  = {Arithmetic}
    (+ (/ (* 2 n) 2) (/ (* (- n 1) n) 2))
  = {Arithmetic}
    (/ (+ (* 2 n) (* n n) (* n -1)) 2)
  = {Arithmetic}
    (/ (+ (* n n) n) 2)
  = {Arithmetic}
    (/ (* n (+ n 1)) 2)
LHS = RHS, so the conjecture is true

All cases have been proven, so the conjecture must be true.


|#
```





