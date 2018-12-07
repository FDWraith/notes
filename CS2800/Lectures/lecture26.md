## Lecture 26: Exam Review

#### Measure Function

```lisp
(defunc f (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract (listp (f x y))
    (cond ((< (len x) (len y)) (f y x))
          ((endp y)                  x)
          (t            (f x (rest y)))))


(f '(1 2 3) '(4 5 6 7 8))
(f '(4 5 6 7) '(1 2 3))
(f '(4 5 6 7) '(2 3))
(f '(4 5 6 7) '(3))
(f '(4 5 6 7) '())

(defunc m (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract (natp (m x y))
    (len y))


#|
Proof Obligations
1. (listp x) /\ (listp y) /\ (< (len x) (len y)) => (m x y) > (m y x)
2. (listp x) /\ (listp y) /\ ~(< (len x) (len y)) /\ ~(endp y) => (m x y) > (m x (rest y))

... Insert Proof Here ...

|#
```

#### Induction

```lisp
#|
(listp x) /\ (listp y) => (perm x y) = (perm y x)

Induction Scheme (for perm):
1. ~((listp x) /\ (listp y)) => phi
2. (listp x) /\ (listp y) /\ (endp x) => phi
3. (listp x) /\ (listp y) /\ ~(endp x) /\ (in (first x) y) /\ phi | ((x (rest x)) (y (del (first x) y))) => phi
4. (listp x) /\ (listp y) /\ ~(endp x) /\ ~(in (first x) y) => phi

Obligation 2
C1. (listp x)
C2. (listp y)
C3. (endp x)
--------------
C4. ~(in e x) {C3, def of in}

LHS:(perm x y)
  = {def of perm, C3}
    (endp y)
CASE 1 : (endp y)
  = {CASE 1}
    t
RHS:(perm y x)
  = {CASE 1}
    (endp x)
  = {C3}
    t
LHS = RHS, so CASE 1 is proven.
CASE 2 : ~(endp y)
  = {CASE 2}
LHS:f
RHS:(perm y x)
  = {CASE 2, def of perm}
    (in (first y) x)
  = {C4}
    f
LHS = RHS, so CASE 2 is proven.
All Cases for Obligation 2 have been proven, so Obligation 2 is proven.

Obligation 3
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
[Let (del (first x) y) be denoted by dy]
C4. (listp (rest x)) /\ (listp dy) => (perm (rest x) dy) = (perm dy (rest x))
C5. (in (first x) y)
----------------
C6. (listp (rest x)) {C1, C3, def of listp}
C7. (listp dy) {Contract theorem of del}
C8. (perm (rest x) dy) = (perm dy (rest x)) {C4, C6, C7, MP}

LHS:(perm x y)
  = {def of perm, C3, C5}
    (perm (rest x) dy)
  = {C8}
    (perm (del (first x) y) (rest x))
RHS:(perm y x)
  = {L1}
    (perm (del (first x) y) (del (first x) x))
  = {def of del}
    (perm (del (first x) y) (rest x))
LHS = RHS, so Obligation 3 is proven.

L1 : (listp x) /\ (listp y) /\ (perm x y) => (perm (del e x) (del e y))

|#
```

