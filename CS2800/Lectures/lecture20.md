## Lecture 20: Lemmas

IS Heuristics

1. Stop $\leftarrow$ think min-l, Not always stop at endp
2. Nothing Free $\leftarrow$ $\phi_{app\_x\_nil}$
3. Recursive Flow $\leftarrow$ Think mergesort
4. K.I.S.S (Keep it simple, stupid) $\leftarrow$ listp / nind
5. Look to the functions 
6. What do I need to know (cases) $\leftarrow$ Proofs with in

```lisp
(defunc subset (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract (booleanp (subset x y))
    (cond 
        ((endp x) t)
        ((in (first x) y) (subset (rest x) y))
        (t nil)))
(defunc in (e x)
    :input-contract (listp x)
    :output-contract (booleanp (in e x))
    (cond 
        ((endp x) nil)
        ((equal e (first x)) t)
        (t (in e (rest x)))))
```

$\phi_{subset\_ref}$ = (listp x) $\Rightarrow$ (subset x x) = true

```lisp
#|

Obligation 1:
C1. (listp x)
C2. (endp x)

  (subset x x)
= {def subset, c2}
  t

Obligation 2:
C1. (listp x)
C2. ~(endp x)
C3. (listp (rest x)) => (subset (rest x) (rest x))
--------------------
C4. (listp (rest x)) {C2, C1, def listp, def endp}
C5. (subset (rest x) (rest x)) {c3, C4, MP}
C6. (in (first x) x) {Def in, first-rest axioms, C2}

LHS:(subset x x)
  = {C2, def subset, C6}
    (subset (rest x) x)
  = {first-rest axiom}
    (subset (rest x) (cons (first x) (rest x)))
  = {L1 | ((x (rest x)) (e (first x)) (y (rest x)))}
    t


L1: (listp x) /\ (listp y) /\ (subset x y) => (subset x (cons e y))
prove L1

|#
```



