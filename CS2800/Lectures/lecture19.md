## Lecture 19: Design Process

Design by Data

1. Data Definitions give rise to predicates recognizing such definitions. This predicate has a <u>recursive scheme</u> that terminates
2. Functions over these data types use the predicate's recursive scheme
3. The Induction Principle: Proofs by induction involving such functions and data definitions should use the same recursion scheme for proof obligations (Induction Schemes)

$\phi_{app\_x\_nil}$ : (listp x) $\Rightarrow$ ((app x nil) = x)

```lisp
#|
1. (endp x) => ((app x nil) x)
2. ~(endp x) /\ phi | ((x (rest x))) => ((app x nil) x)

Case 1.
C1. (listp x)
C2. (endp x)
------------
C3. x = nil {Def of endp, C2}

LHS:(app x nil)
 = {def of app, C2}
   nil

LHS = RHS, so Case 1 is t.

Case 2.
C1. (listp x)
C2. ~(endp x)
C3. (listp (rest x)) => ((app (rest x) nil) = (rest x))
---------------
C4. (consp x) {C1, def of endp}
C5. (listp (rest x)) {C1, C4, Def of listp, cons axiom}
C6. ((app (rest x) nil) = (rest x)) {C3, C5, MP}

LHS:(app x nil)
 = {Def of app, C2}
   (cons (first x) (app (rest x) nil))
 = {C6}
   (cons (first x) (rest x))
 = {first-rest axiom}
   x

LHS = RHS, so Case 2 is t.
|#
```



