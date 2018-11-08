## Lecture 23: Tail Recursion (Con't)

```lisp
#|
Lemma 1: (listp l) /\ (natp acc) => (len-t l acc) = (+ acc (len l))

Induction Scheme (from len-t):
1. ~IC => phi
2. IC /\ (endp l) => phi
3. IC /\ ~(endp l) /\ phi | ((l (rest l)) (acc (+ 1 acc))) => phi

Obligation 1:
C1. ~IC
C2. IC
------------
C3. f {C1, C2, PL}

f => phi is t, so Obligation 1 is proven

Obligation 2:
C1. (listp l)
C2. (natp acc)
C3. (endp l)
--------------

LHS:(len-t l acc)
  = {def of len-t, C1}
    acc
RHS:(+ acc (len l))
  = {def of len, C3}
    (+ acc 0)
  = {Arithmetic} 
    acc

LHS = RHS, so Obligation 2 is proven

Obligation 3:
C1. (listp l)
C2. (natp acc)
C3. ~(endp l)
C4. (listp (rest l)) /\ (natp (+ 1 acc)) => (len-t (rest l) (+ 1 acc)) = (+ (+ 1 acc) (len (rest l)))
--------------
C5. (listp (rest l)) {C1, C3, def of listp, def of endp}
C6. (natp (+ 1 acc)) {C2, def of natp}
C7. (len-t (rest l) (+ 1 acc)) = (+ (+ 1 acc) (len (rest l))) {C4, C5, C6, MP}

LHS:(len-t l acc)
  = {def of len-t, C3}
    (len-t (rest l) (+ 1 acc))
  = {C7}
    (+ (+ 1 acc) (len (rest l)))
RHS:(+ acc (len l))
  = {def of len, C3}
    (+ acc (+ 1 (len (rest l))))
  = {Arithmetic}
    (+ (+ 1 acc) (len (rest l)))

LHS = RHS, so Obligation 3 is proven

|#
```

```lisp
(defunc add-lists (l1 l2)
    :input-contract (and (lorp l1) (lorp l2))
    :output-contract (lorp (add-lists l1 l2))
    (cond
        ((endp l1) l2)
        ((endp l2) l1)
        (t (cons (+ (first l1) (first l2))
           	     (add-lists (rest l1) (rest l2))))))
(defunc add-lists* (l1 l2)
    :input-contract (and (lorp l1) (lorp l2))
    :output-contract (lorp (add-lists* l1 l2))
    (rev (add-lists-t l1 l2 nil))))

;; Write add-lists-t
(defunc add-lists-t (l1 l2 acc)
    :input-contract (and (lorp l1) (lorp l2) (lorp acc))
    :output-contract (lorpl (add-lists-t l1 l2 acc))
    (cond
        ((end l1) (app (rev l2) acc))
        ((end l2) (app (rev l1) acc))
        (t (add-lists-t (rest l1) (rest l2) (cons (+ (first l1) (first l2)) acc)))))

#|
Prove: (lorp l1) /\ (lorp l2) => (add-lists* l1 l2) = (add-lists l1 l2)

Induction Scheme:
1. ~IC => phi
2. IC /\ (endp l1) => phi
3. IC /\ ~(endp l1) /\ (endp l2) => phi
4. IC /\ ~(endp l1) /\ ~(endp l2) /\ phi | ((l1 (rest l1)) (l2 (rest l2))) => phi

Obligation 1:
C1. ~IC
C2. IC 
--------
C3. f {C1, C2, PL}

f => phi is t, so Obligation 1 is proven.

Obligation 2: 
C1. (lorp l1)
C2. (lorp l2)
C3. (endp l1)
--------------

LHS:(add-lists* l1 l2)
  = {def of add-lists*}
    (rev (add-lists-t l1 l2 nil))
  = {def of add-lists-t, C3}
    (rev (app (rev l2) nil))
  = {phi_app_nil | (x (rev l2))}
    (rev (rev l2))
  = {phi_rev_rev}
    l2
RHS:(add-lists l1 l2)
  = {def of add-lists, C3}
    l2

LHS = RHS, so Obligation 2 is proven.

Obligation 3: 
C1. (lorp l1)
C2. (lorp l2)
C3. ~(endp l1)
C4. (endp l2)
--------------

LHS:(add-lists* l1 l2)
  = {def of add-lists*}
    (rev (add-lists-t l1 l2 nil))
  = {def of add-lists-t, C3}
    (rev (app (rev l2) nil))
  = {phi_app_nil | (x (rev l2))}
    (rev (rev l2))
  = {phi_rev_rev}
    l2
RHS:(add-lists l1 l2)
  = {def of add-lists, C3}
    l2

LHS = RHS, so Obligation 2 is proven.



=============

Lemma: (add-lists-t l1 l2 acc) = (app (rev (add-lists l1 l2)) acc)

Induction Scheme:
1. ~IC => phi
2. IC /\ (endp l1) => phi
3. IC /\ ~(endp l1) /\ (endp l2) => phi
4. IC /\ ~(endp l1) /\ ~(endp l2) /\ phi | ((l1 (rest l1)) (l2 (rest l2)) (acc (cons (+ (first l1) (first l2)) acc))) => phi




|#

```

