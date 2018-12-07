## Lecture 22: Tail-Recursion Recipe

```lisp
(defunc rev-t (x acc)
    :input-contract (and (listp x) (listp acc))
    :output-contract (listp (rev-t x acc))
    (if (endp x)
        acc
        (rev-t (rest x) (cons (first x) acc))))
(defunc rev (x)
    :input-contract (listp x)
    :output-contract (listp (rev x))
    (if (endp x)
        x
        (app (rev (rest x)) (list (first x)))))

#|
Lemma 1: (listp x) /\ (listp acc) => (rev-t x acc) = (app (rev x) acc)

Induction Scheme:
1. ~(IC) => phi
2. IC /\ (endp x) => phi
3. IC /\ ~(endp x) /\ phi | ((x (rest x)) (acc (cons (first x) acc))) => phi

Obligation 1:
C1. IC
C2. ~IC
---------
C3. f {C1, C2, PL}

f => phi

Obligation 2:
C1. IC
C2. (endp x)
C3. (listp acc)
--------------

LHS:(rev-t x acc)
  = {def of rev, C2}
    acc
RHS:(app (rev x) acc)
  = {def of rev, C2}
    (app nil acc)
  = {def of app}
	acc

LHS = RHS, so Obligation 2 is proven.

Obligation 3:
C1. (listp x)
C2. (listp acc)
C3. ~(endp x)
C4. (listp (rest x)) /\ (listp (cons (first x) acc)) => (rev-t (rest x) (cons (first x) acc)) = (app (rev (rest x)) (cons (first x) acc))
----------------------
C5. (listp (rest x)) {C1, C3, def of endp, def of listp}
C6. (listp (cons (first x) acc)) {def of listp, C2}
C7. (rev-t (rest x) (cons (first x) acc)) = (app (rev (rest x)) (cons (first x) acc)) {C4, C5, C6, MP}

LHS:(rev-t x acc)
  = {def of rev-t, C3}
    (rev-t (rest x) (cons (first x) acc))
RHS:(app (rev x) acc)
  = {def of rev, C3}
    (app (app (rev (rest x)) (list (first x))) acc)
  = {associativty of app}
    (app (rev (rest x)) (app (list (first x)) acc))
  = {def of app, def of endp, consp axiom, first-rest axiom}
    (app (rev (rest x)) (cons (first x) (app nil acc)))
  = {def of app, def of endp, consp axiom}
    (app (rev (rest x)) (cons (first x) acc))
  = {C7}
    (rev-t (rest x) (cons (first x) acc))

LHS = RHS, so Obligation 3 is proven.
|#
```

### Accumulator Proof Recipe

##### Part 1

1. Write a function `f`
2. Write a tail-recursive function `f-t` such that it calculates the same thing as `f` but it uses an accumulator
3. Write a non-recursive function `f*` that has the same IC and OC as `f` and outputs the same value as `f`

##### Part 2

4. Play with some values
5. Write a lemma relating `f-t`, the accumulator and `f`. There should be no constants.
6. Prove `f* = f` using the lemma from 5.
7. Prove the lemma from 5 using the induction scheme that `f-t` gives rise to.
8. Prove any additional lemmas you used.

```lisp
(defunc len (l)
    :input-contract (listp l)
    :output-contract (natp (len l))
    (if (endp l)
        0
        (+ 1 (len (rest l)))))

(defunc len-t (l acc)
    :input-contract (and (listp l) (natp acc))
    :output-contract (natp (len-t l acc))
    (if (endp l)
        acc
        (len-t (rest l) (+ 1 acc))))

(defunc len* (l)
    :input-contract (listp l)
    :output-contract (natp (len* l))
    (len-t l 0))

#|
Prove: (listp l) => (len* l) = (len l)

Obligation 1:
C1. ~(listp l)
C2. (listp l)
-------------
C3. nil {C1, C2, PL}

Obligation 2:
C1. (listp l)
C2. (endp l)
-----------

LHS:(len* l)
  = {def of len*}
    (len-t l 0)
  = {def of len-t, C2}
    0
RHS:(len l)
  = {def of len, C2}
    0

LHS = RHS, so Obligation 2 is proven.


Obligation 3:
C1. (listp l)
C2. ~(endp l)
C3. (listp (rest l)) => (len* (rest l)) = (len (rest l))
-------------- 

LHS:(len* l)
  = {def of len*}
    (len-t l 0)
  = {L1 | ((acc 0)))}
    (+ 0 (len l))
  = (len l)
RHS:(len l)

Lemma 1: (listp l) /\ (natp acc) => (len-t l acc) = (+ acc (len l))

Induction Scheme (from len-t):
1. ~IC => phi
2. IC /\ (endp l) => phi
3. IC /\ ~(endp l) /\ phi | ((l (rest l)) (acc (+ 1 acc))) => phi



|#
```

