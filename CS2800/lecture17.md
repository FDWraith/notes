## Lecture 17: Induction Schemes

```lisp
(defunc nind (n)
    :input-contract (natp n)
    :output-contract (natp (nind n))
    (if (equal n 0)
        0
        (nind (- n 1))))
```

<u>Induction Scheme `nind` gives rise to:</u>

1. ~(natp n) $\Rightarrow$ $\phi$
2. (natp n) $\and$ (n $=$ 0) $\Rightarrow$ $\phi$
3. (natp n) $\and$ (n $\neq$ 0) $\and$ $\phi|_{\text{((n (- n 1)))}}$ $\Rightarrow$ $\phi$

```lisp
(defunc f (x1 x2 x3 ... xn)
    :input-contract IC
    :output-contract OC
    (cond (e1 x1)
          (e2 x2)
          (e3 x3) ;; Has a recursive call
            ...
          (en xn)
          (t  xn+1)))
```

<u>Induction Scheme for `f` is:</u>

1. ~IC $\Rightarrow$ $\phi$
2. IC $\and$ e1 $\Rightarrow$ $\phi$
3. IC $\and$ ~e1 $\and$ e2 $\Rightarrow$ $\phi$
4. IC $\and$ ~e1 $\and$ ~e2 $\and$ e3 $\and$ $\phi|_{\text{X3}}$ $\Rightarrow$ $\phi$
5. ​             $\vdots$
6. IC $\and$ ~e1 $\and$ ~e2 $\and$ ~e3 $\and$ $\dots$ $\and$ ~en $\Rightarrow$ $\phi$

```lisp
(defunc prod-l (l)
    :input-contract (and (consp l) (lorp l))
    :output-contract ...
    (cond 
        ((endp (rest l)) (first l))
        (t (* (first l) (prod-l (rest l))))))
```

