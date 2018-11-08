## Lecture 21: Proof with Accumulators

```lisp
#|
(defunc rev* (x)
	:input-contract (listp x)
	:output-contract (listp (rev* x))
	(rev-t x nil))

Prove (listp x) => (rev x) = (rev* x)

Lemma 1:
(listp acc) /\ 
|#
```



