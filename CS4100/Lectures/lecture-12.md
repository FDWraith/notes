## Lecture 12: Bayesian Networks

Keep in mind:

$P(Y) = \sum_{z \in Z} P(Y,z)$

$P(X|Y) = \frac{P(X,Y)}{P(Y)}$

$P(X_n,X_{n-1},...,X_1) = P(X_n|X_{n-1}, X_{n-2}, ..., X_1)P(X_{n-1}|X_{n-2},...,X_1)...P(X_1)$

$P(X|Y) = \frac{P(Y|X)P(X)}{P(Y)}$

#### Bayesian Network

- probabilistic model of a domain
- each node represents random variable
- directed acyclic graph (DAG)
- Each node $X_i$ has conditional distribution

Construction:

- simplify factors of chain rule using conditional independence
- For each node $X_i$, find the minimal set of parents for $X_i$
  - $P(X_i | X_{i-1}, ..., X_1) = P(X_i | \text{Parents(}X_i))$
- Expand out full joint table
  - apply marginalization to reduce terms
- Variable elimination -- reorder to reduce variable dependence

#### Approximate Inference by Sampling

- getting a sample is faster than computing exact answer (even with variable elimination)

- sample using value from [0, 1], and use sub-intervals to represent different probabilities.

- sampling done many times

- query probabilities is counting sample results.

- rejection sampling

  - stop current sample when it doesn't match query
  - problem is that this rejects a lot of samples

- likelihood sampling

  - fix evidence variables and sample the rest
  - problem: samples distribution not consistent with original distribution
  - solution: weight samples by probability given parents

  



