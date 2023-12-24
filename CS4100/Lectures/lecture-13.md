## Lecture 13: HMMs

**Markov Chain**

- when predicting future, the past doesn't matter, only the present
- $P(q_i = a | q_1 ... q_{i-1}) = P(q_i = a|q_{i-1})$
- Assumes stationary chain:
  - $P(X_{i+1} | X_i) = P(X_{j+1} | X_j) \space \forall i,j > 0$
  - eg. probability doesn't change per chain segment

**Hidden Markov Model (HMM)**

- Making assumptions on underlying markov chain based on observations in world
- Prediction:
  - derive expected outcomes based on current states

**Parameters** ($\lambda = (A,B, \pi)$)

- A matrix of transition probabilities between hidden states, $P(s_i | s_j)$
- B matrix of observation probabilities $P(v_m | s_i)$ where $v_m$ is value of observations
- $\pi$ initial probabilities for each state

**Supervised Learning**

- given several examples with observation and hidden state sequences

**Unsupervised learning**

- only given examples of observation sequences -- need to figure out hidden state sequences
- Baum-Welch Algorithm (Forward/Backward)
  - forward probabilities (estimate current state, given all evidence/observation to date)
  - backward probabilities (compute posterior distribution over past states, given evidence up to present)
  - combine two to converge faster
- $a_{i,j} = \frac{\text{number of transitions from state i to state j}}{\text{number of transitions from start i}}$
- Algorithm:
  - Assume an HMM with $n$ states
  - Randomly set parameters $\lambda = (A,B,\pi)$
  - Re-estimate parameters using observation data until $\lambda$ converges



