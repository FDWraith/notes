## Lecture 9

### Lambda Calculus

- Notation for expressing functions in languages
- Eg. `double x` can be represented as $$\lambda x. \hspace{10px} x \cdot x$$
- Functions as values

```haskell
<Lambda> ::= <Variable>
		   | (λ <Variable>. <Lambda>)
		   | (<Lambda> <Lambda>)
```

- $\lambda x $ is the binder 
- Conventions
  - drop outermost parens $(\lambda x. \hspace{10px} x x) \Rightarrow \lambda x. \hspace{10px} x x$ 
  - lambdas extend to the right $(\lambda x. (\lambda y. (x y))) \Rightarrow \lambda x. \lambda y. \hspace{10px} x y$

$\beta$-reduction:

- $((\lambda x. M) N) \rightarrow _{\beta} M[x::=N]$ 
- Some expressions are reducible, and referred to as redex

Equivalence

- Two functions are considered the same when they produce the same output for the same input over all possible inputs
- 

