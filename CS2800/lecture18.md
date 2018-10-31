## Lecture 18: The Monster

$\phi_{rev\_rev}$ : (listp x) $\Rightarrow$ (rev (rev x)) = x

1. (endp x) $\Rightarrow$ $\phi_{rev\_rev}$
2. ~(endp x) $\and$ $\phi_{rev\_rev}$ | ((x (rest x)))  $\Rightarrow$ $\phi_{rev\_rev}$

Obligation 1:

```
C1. (listp x)
C2. (endp x)

  (rev (rev x))
= {Def of rev, C2}
  (rev x)
= {Def of rev, C2}
  x
```

Obligation 2:

```
C1. (listp x)
C2. ~(endp x)
C3. (listp (rest x)) => (rev (rev (rest x))) = (rest x)
------------------
C4. (listp (rest x)) {C1, Def endp, C2, Def listp}
C5. (rev (rev (rest x))) = (rest x)
  
  
  (rev (rev x))
= {Def of rev, C2}
  (rev (app (rev (rest x)) (list (first x))))
= {Def of rev}
  (rev (app (rev (rest x)) (rev (list (first x)))))
= {Lemma}
  (app (rev (list (first x)) (rev (rev (rest x))))
= {C5}
  (app (rev (list (first x)) (rest x)))
= {Def of rev, consp axiom, consp axiom, first-rest axiom}
  (app (app (rev niL) (list (first x))) (rest x))
= {def rev, def endp}
  (app (app nil (list (first x))) (rest x))
= {Def endp, def app}
  (app (list (first x)) (rest x))
= {Def app}
  (cons (first x) (app nil (rest x)))
= {Def app, def of endp}
  (cons (first x) (rest x))
= {first-rest axiom}
  x
```

Lemma $\phi_{rev\_app}$ : (listp a) $\and$ (listp b) $\Rightarrow$ (rev (app a b)) = (app (rev b) (rev a)) 

```

```



