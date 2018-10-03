## Lecture 11: Proofing

**Substitution** can only replace variables, not function names

**Given:** A => B

**Proof:** Assume A, show B must be true because of A

**Usage**: Show A, B must follow because of Proof.

**Theorem 4.1**

```(listp z) (listp y) => ((cons x (app y z)) = (app (cons x y) z))```

---

```
(implies (and (consp x)
			  (implies (and (listp (rest x)) (listp y) (listp z))
			  		   (equal (app (rest x) (app y z))
			  		          (app (app (rest x) y))))
	     (equal (app x (app y z))
	            (app (app x y) z)))
```

**Context**

C1. ```(listp x)```

C2. ```(listp y)```

C3. ```(listp z)```

C4. ```(consp x)```

C5. ```(listp (rest x)) ^ (listp y) ^ (listp z) => (app (rest x) (app y z)) = (app (app (rest x) y) z)```

C6. ```~(endp x)``` [Defn ```endp``` from C4]

C7. ```(listp (rest x))``` [Defn ```listp``` from C4]

C8. ```(app (rest x) (app y z)) = (app (app (rest x) y) z)``` [C2, C3, C7, C5, MP] 

**Proof**

```
LHS: (app x (app y z))
   = {Defn app | ((x x) (y (app y z))), if axiom, C6}
     (cons (first x) (app (rest x) (app y z)))
   = {C8}
     (cons (first x) (app (app (rest x) y z)))
   = {Theorem 4.1}
     (app (cons (first x) (app (rest x) y) z))
   = {Defn app, C6, if axiom}
     (app (app x y) z)
   = {QED}
```

