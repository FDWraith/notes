## Lecture 12: Modus Tollens

**Contrapositive**: ```A => B = ~B => ~A```

**Example**:

```lisp
(implies (endp x)
         (implies (and (listp x) (listp y))
                  (implies (not (in a (app x y)))
                           (not (in a x)))))
; Rewrite (Contrapositive + Exportation)
(implies (and (listp x) (listp y) (endp x) (in a x))
         (in a (app x y)))
```

**Proof**

```
C1. (listp x)
C2. (listp y)
C3. (endp x)
C4. (in a x)
C5. ~(in a x) {Defn in, C3, if axiom}
C6. nil {C4, C5}
    nil => \phi is true
```

**Part 2**

```lisp
(implies (and (listp x) (listp y) (not (endp x)) (equal (first x) a) (in a x))
         (in a (app x y)))
```

**Proof**

```
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
C4. (first x) = a
C5. (in a x)
------------

  (in (app x y))
= {Defn app}
  (in (if (endp x) y (cons (first x) (app (rest x) y))))
= {if axiom, C3}
  (in (cons (first x) (app (rest x) y)))
= {Defn in | ((x (cons (first x) (app (rest x) y))) (e a)), Defn endp, consp axiom}
  (if (equal (first (cons (first x) (app (rest x) y)))
       t 
      (in a (rest (cons (first x) (app (rest x) y)))))
= {First-Rest-axiom, C4, if axiom}
  t 
```

**Part 3**

```lisp
(implies (and (listp x) (listp y) (not (endp x)) (not (equal (first x) a)) (in a x))
         (in a (app x y)))

; Inductive Assumption
(implies (and (listp (rest x)) (listp y) (in a (rest x)))
         (in a (app (rest x) y)))
```

**Proof**

```
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
C4. ~((first x) = a)
C5. (in a x)
C6. (listp (rest x)) ^ (listp y) ^ (in a (rest x)) => (in a (app (rest x) y))
-----------
C7. (listp (rest x)) {C1, C3, Defn listp}
C8. (in a (rest x)) {Defn in, if axiom, C3, C4, C5, PL}


  (in a (app x y))
= {Defn app, C3}
  (in a (cons (first x) (app (rest x) y))
= {Defn in, Defn endp, consp axiom, first-rest-axioms}
  (if (equal a (first x)) t (in a (app)))
```

