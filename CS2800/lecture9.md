## Lecture 8: Boolean Satisfiability and NP-Completeness

#### Boolean Satisfiability

```
(a ^ b ^ c) v (d ^ ~a ^ b) v ...
```

$$
\begin{align*}
&P \longrightarrow O(n^2) &\quad \text{Polynomial Time} \\
&NP  \longrightarrow O(2^n) &\quad \text{Non-deterministic Polynomial Time}
\end{align*}
$$

#### P = NP

If ```P=NP``` then all NP-complete problems can be solved in the same manner.
$$
\text{Problem 1} \underbrace{\longrightarrow}_{P} \text{Sat Problem}
$$

```
Valid(p) 
Falsifiable(p) = (not (valid p))
Unsatisfiable(p) = (valid (not p))
Satisfiable(p) = (not (valid (not p)))
```

#### Proof Techniques

<u>Logical Equivalence:</u> 
$$
\begin{align*}
& a \and b \equiv a \equiv a \or b \\
\equiv & \text{{Commutative, Associative}} \\
 & (a \and b) \equiv (a \or b) \equiv a \equiv b \\
\equiv & \{\text{L1}\} \\
 & true
\end{align*}
$$


Prove L1: 
$$
((a \and b) \equiv (a \or b)) \equiv a \equiv b
$$
using truth tables:

| a    | b    | a ^ b | a v b | (a ^ b) = (a v b) | a = b |
| ---- | ---- | ----- | ----- | ----------------- | ----- |
| f    | f    | f     | f     | t                 | t     |
| f    | t    | f     | t     | f                 | f     |
| t    | f    | f     | t     | f                 | f     |
| t    | t    | t     | t     | t                 | t     |

<u>Proof By Cases</u>
$$
p \and (p \or q) \equiv p \quad \text{{Absorption}} \\
\begin{align*}
\textbf{Case 1:} \quad &\text{p = true} \\
& (\text{true}) \and \underline{(\text{true} \or q)} \\
\equiv &\text{{Annihilator}} \\
&\text{true} \\
\textbf{Case 2:} \quad &\text{p = false} \\
& \underline{(\text{false}) \and (\text{false} \or q)} \\
\equiv &\text{{Annihilator}} \\
&\text{false} \\
\end{align*}
$$
<u>Instantiation</u>

Prove 
$$
p \oplus q \oplus false \equiv p \oplus q
$$
Substitute a theorem to prove another theorem
$$
\begin{align*}
p \oplus false \equiv p \quad &\vert \\
                        &\vert \quad \underbrace{p}_{atom} \longrightarrow \underbrace{p \oplus q}_{image}
\end{align*}
$$




