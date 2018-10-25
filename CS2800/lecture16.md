## Lecture 16: Induction Schemes

```lisp
(defunc f (x y)
	:input-contract (and (integerp x) (natp y))
	:output-contract (integerp (f x y))
	(cond ((equal x 0) 1)
	      ((< x 0) (f (+ 1 y) (* x x)))
	      (t (+ 1 (f (- x 1) y)))))

(defunc m-f (x y)
    :input-contract (and (integerp x) (natp y))
    :output-contract (natp (m-f x y))
    (if (>= x 0)
        x
        (+ y 2)))
#|
proof obligations:
(integerp x) /\ (natp y) /\ (< x 0) /\ ~(x = 0) => (m-f x y) > (m-f (+ 1 y) (* x x))
(integerp x) /\ (natp y) /\ ~(< x 0) /\ ~(x = 0) => (m-f x y) > (m-f (- x 1) y)

Case 1:
C1. (integerp x)
C2. (natp y)
C3. (< x 0)
C4. ~(x = 0)
----------------
C5. (+ y 1) > 0 {C2, Def of nat}

LHS:(m-f x y)
  = {Def of m-f, C3, if axiom}
    (+ y 2) 
RHS:(m-f (+ 1 y) (* x x))
  = {Def of m-f, C5, if axiom}
    (+ 1 y)
LHS > RHS 
|#
```

```lisp
(defunc nind (n)
    :input-contract (natp n)
    :output-contract (natp (nind n))
    (if (equal n 0) 0 (nind (- n 1))))
```

#### Induction Scheme

```nind``` gives rise to:

1. ~(natp n) $\Rightarrow$ $\phi$
2. (natp n) $\and$ (n = 0) $\Rightarrow$ $\phi$
3. (natp n) $\and$ (n $\neq$ 0) $\and$ $\phi$ | (n (- n 1)) $\Rightarrow$ $\phi$



