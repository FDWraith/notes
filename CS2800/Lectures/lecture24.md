## Lecture 24: More Tail Recursion

```lisp
(defunc insert (e l)
    :input-contract (and (rationalp e) (lorp l))
    :output-contract (lorp (insert e l))
    (cons ((endp l) (list e))
          ((<= e (first l)) (cons e l))
          (t (cons (first l) (insert e (rest l))))))

(defunc isort (l)
    :input-contract (lorp l)
    :ouptut-contract (lorp (isort l))
    (if (endp l)
        nil
        (insert (first l) (isort (rest l)))))
```

#### From Last Time

```lisp
#|
Lemma: (lorp l1) /\ (lorp l2) /\ (lorp acc) => (add-lists-t l1 l2 acc) = (app (rev (add-lists l1 l2)) acc)

Induction Scheme:
1. ~IC => phi
2. IC /\ (endp l1) => phi
3. IC /\ ~(endp l1) /\ (endp l2) => phi
4. IC /\ ~(endp l1) /\ ~(endp l2) /\ phi | ((l1 (rest l1)) (l2 (rest l2)) (acc (cons (+ (first l1) (first l2)) acc))) => phi

Obligation 1:
C1. ~((lorp l1) /\ (lorp l2) /\ (lorp acc))
C2. (lorp l1)
C3. (lorp l2)
C4. (lorp acc)
---------------------
C5. f {C1, C2, C3, C4, PL}

f => phi is t, so Obligaion 1 is proven.

Obligation 2:
C1. (lorp l1)
C2. (lorp l2)
C3. (lorp acc)
C4. (endp l1)
----------------

LHS:(add-lists-t l1 l2 acc)
  = {def of add-lists-t, C4}
    (app (rev l2) acc)
RHS:(app (rev (add-lists l1 l2)) acc)
  = {def of add-lists, C4}
    (app (rev l2) acc)

LHS = RHS, so Obligation 2 is proven.

Obligation 3:
C1. (lorp l1)
C2. (lorp l2)
C3. (lorp acc)
C4. ~(endp l1)
C5. (endp l2)
-----------------

LHS:(add-lists-t l1 l2 acc)
  = {def of add-lists-t, C4, C5}
    (app (rev l1) acc)
RHS:(app (rev (add-lists l1 l2)) acc)
  = {def of add-lists, C4, C5}
    (app (rev l1) acc)

LHS = RHS, so Obligation 3 is proven.

Obligation 4:
C1. (lorp l1)
C2. (lorp l2)
C3. (lorp acc)
C4. ~(endp l1)
C5. ~(endp l2)
C6. (lorp (rest l1)) /\ (lorp (rest l2)) /\ (lorp (cons (+ (first l1) (first l2)) acc)) => (add-lists-t (rest l1) (rest l2) (cons (+ (first l1) (first l2)) acc)) = (app (rev (add-lists (rest l1) (rest l2))) (cons (+ (first l1) (first l2)) acc))
-------------------
C7. (lorp (rest l1)) {C1, C4, def of lorp}
C8. (lorp (rest l2)) {C2, C5, def of lorp}
C9. (rationalp (first l1)) {C1, C4, def of lorp, first-rest axiom}
C10. (rationalp (first l2)) {C2, C5, def of lorp, first-rest axiom}
C11. (rationalp (+ (first l1) (first l2))) {C9, C10}
C12. (lorp (cons (+ (first l1) (first l2)) acc)) {C11, C3, def of lorp, first-rest}
C13. (add-lists-t (rest l1) (rest l2) (cons (+ (first l1) (first l2)) acc)) = (app (rev (add-lists (rest l1) (rest l2))) (cons (+ (first l1) (first l2)) acc)) {C6, C7, C8, C12, MP} [Call this expression expr_a = expr_b]

LHS:(add-lists-t l1 l2 acc)
  = {def of add-lists-t, C4, C5}
    expr_a
  = {C13}
    expr_b
RHS:(app (rev (add-lists l1 l2)) acc)
  = {def of add-lists, C4, C5}
    (app (rev (cons (+ (first l1) (first l2)) (add-lists (rest l1) (rest l2)))) acc)
  = {def of rev, def of endp, consp axiom, first-rest axiom}
    (app (app (rev (add-lists (rest l1) (rest l2))) (list (+ (first l1) (first l2))) acc)
  = {associativity of app}
    (app (rev (add-lists (rest l1) (rest l2))) (app (list (+ (first l1) (first l2)) acc)))
  = {def of app, def of endp, consp axiom, first-rest axiom}
    expr_b

LHS = RHS, so Obligation 4 is proven.

All Obligations have been proven, so Lemma is t.


|#
```



