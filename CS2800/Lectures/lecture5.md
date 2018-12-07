## Lecture 5: Defining Data

```lisp
(defunc sum-n (n)
	:input-contract (natp n)
	:output-contract (natp (sum-n n))
	(if (equal n 0)
		0
		(+ n (sum-n (+ n -1)))))
```

General Form for Contracts

```lisp
(defunc f (x1 x2 .... xn)
	:input-contract (and (R1 x1) (R2 x2) .... (Rn xn) Po Pp)
	:output-contract (R (f x1 x2 .... xn))
	;; Function Body
	)
```

Let allows local definitions

```lisp
(let ((val1 (+ 3 4))
      (val2 (+ 5 6))
      (val3 '(why does this seem complicated)))
	(+ val1 val2))
```

```lisp
(def data lor (listof rational))
(defunc min-l (l)
	:input-contract (and (lorp l) (consp 1))
	:ouput-contract (rationalp (min-l l))
	(if (endp (rest l))
		(first l)
		(let ((mr (min-l (rest l))))
			(if (< (first l) mr)
				(first l)
				mr))))

;; Refer below for information on test?
(test? (implies (and (rationalp r) (lorp l) (in r l)) 
                (<= (min-l) r)))
(test? (implies (and (lorp l) (consp l)) 
                (in (min-l l) l)))
```

**Drawback**: Let does its local definitions in parallel, so you can't use its own local definitions in let

**Workaround:** Use let*

```lisp
(defunc foo ()
	:input-contract t
	:output-contract (listp (foo))
	(let* ((q '(a b c))
		   (r '(c d))
		   (s (app q q))
		   (v (app r r)))
		(app s v)))
```

```even-natp```

```lisp
(defunc even-natp (n)
	:input-contract t
	:output-contract (booleanp (even-natp n))
	(if (natp n) 
		(natp (/ n 2))
        nil))

; Data-Driven Definition
(defunc even-nat2p (n)
    :input-contract t
    :output-contract (booleanp (even-nat2p n))
    (cond ((equal n 0) t)
          ((natp n) (not (even-nat2p (- n 1))))
          (t nil)))

; Test to see if two functions behave identically
(test? (implies (natp n) (equal (even-nat2p n) (even-natp n))))
; Uses the Theorem Prover to prove that this statement is true
(thm (implies (natp n) (equal (even-nat2p n) (even-natp n))))
```



