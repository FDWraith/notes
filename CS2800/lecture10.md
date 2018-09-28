## Lecture 10: Intro to Equational Reasoning

**Conjecture**: Assumption we are trying to prove

**Theorem**: Proven conjecture that is always true and valid.

**Tautology**: True statements

**Lemma**: Helper Theorem to help prove main Theorem

**Corrollary**: Side effect of proving a Theorem

**Axiom**: Statements assumed to be true

**Conjecture 1**
$$
p \in \N \and q \in \N \\
p + q = p * q
$$
The statement is falsifiable when p = 2, and q = 3.

**Conjecture 2**
$$
p \in \N \and q \in \N \\
p + q = q + p
$$
Cannot prove or disprove, because p and q belong in infinite sets (Cannot Brute Force)

**Limits of Propositional Logic**
$$
\underbrace{(\text{listp } x) \and (\text{listp } y) \and (\text{listp } z)}_{context} \Rightarrow [(\text{app } x \space (\text{app } y \space z)) = (\text{app } (\text{app } x \space y)\space z))]
$$

```lisp
(defunc alen (l)
    :input-contract t
    :output-contract (natp (alen l))
    (if (atom l)
        0
        (+ 1 (alen (rest l)))))

(defunc atom (x)
    :input-contract t
    :output-contract (booleanp (atom x))
    (not (consp x)))

(defunc not (a)
    :input-contract (booleanp a)
    :output-contract (booleanp (not a))
    (if nil t nil))
```

$$
\begin{align*}
\textbf{Conjecture 4: } &\text{(alen (cons x z)) = (alen (cons y z))} \\
\\
\textbf{Lemma 1: } &\text{(alen (cons x z)) } \equiv \text{(+ 1 (alen z))}\\
&\text{(alen (cons x z))} \\
\equiv \space &\text{{def of cons | ((l (cons x z)))}} \\
&\text{(if (atom (cons x z)) 0 (+ 1 (alen (rest (cons x z))))} \\
\equiv \space &\text{{def of atom | ((x (cons x z)))}} \\
&\text{(if (not (consp (cons x z))) 0 (+ 1 (alen (rest (cons x z))))} \\
\equiv \space &\text{{consp axiom}} \\
&\text{(if nil 0 (+ 1 (alen (rest (cons x z))))} \\
\equiv \space &\text{{if axiom}} \\
&\text{(+ 1 (alen (rest (cons x z)))} \\
\equiv \space &\text{{rest axiom}} \\
&\text{(+ 1 (alen z))} \quad \checkmark\\
\\
\textbf{Conjecture 4: } &\text{(alen (cons x z)) = (alen (cons y z))} \\
\equiv \space &\text{{L1}} \\
&\text{(+ 1 (alen z)) = (alen (cons y z))} \\
\equiv \space &\text{{L1}} \\
&\text{(+ 1 (alen z)) = (+ 1 (alen z))} \quad \checkmark \\
\end{align*}
$$

